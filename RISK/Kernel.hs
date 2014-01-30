{-# LANGUAGE TupleSections #-}
module RISK.Kernel
  ( Intrinsic (..)
  , kernelProgram
  ) where

import Data.Word
import Language.GIGL
import Text.Printf

import RISK.Config
import RISK.Spec

type RISK = GIGL (Spec, Config) Intrinsic

data Intrinsic
  -- | IOInterruptHandler
  -- | PartitionExceptionHandler
  = SetMemoryPtrs
  | SaveContext    String
  | RestoreContext String
  | StartPartition String
  | Return
  | ExitSimulation
  | TransferMessages String Integer String Integer  -- ^ Transfer messages from one partition to another.
  deriving (Show, Eq)

-- | Builds the kernel from the kernel specification.
kernelProgram :: Spec -> Program Intrinsic
kernelProgram spec = snd $ elaborate (spec, configure spec) kernel

-- Get the kernel spec.
spec :: RISK Spec
spec = getMeta >>= return . fst

-- Get the kernel configuration.
config :: RISK Config
config = getMeta >>= return . snd

-- The kernel.
kernel :: RISK ()
kernel = do
  declareKernelInit
  declareKernelEntry

interruptSource :: E Word64
interruptSource = Var "risk_interrupt_source"

interruptSourceIsYield :: E Bool
interruptSourceIsYield = interruptSource .== Const 1

activePartition :: E Word64
activePartition = Var "risk_active_partition"

schedulingPhase :: E Word64
schedulingPhase = Var "risk_scheduling_phase"

cycleCount :: E Word64
cycleCount = word64 "risk_cycle_count"

-- Kernel initialization.
declareKernelInit :: RISK ()
declareKernelInit = proc "risk_init" $ do
  config <- config
  comment "Initialize the active partition and the scheduling phase to numbers outside the valid range."
  activePartition <== Const 0xffffffffffffffff
  schedulingPhase <== Const 0xffffffffffffffff
  comment "Initialize the partition init flags."
  sequence_ [ bool (name ++ "_initialized")  <== false |  name <- partitionNames config ]
  comment "Initialize the partition memory pointers."
  intrinsic SetMemoryPtrs
  comment "Initialize the partition stack pointers."
  sequence_ [ word64 (name ++ "_stack_ptr") <== Add (word64 $ name ++ "_memory_ptr") (Const $ fromIntegral $ partitionMemorySize config name) | name <- partitionNames config ]
  comment "Initialize the partition channel buffer and data region pointers."
  mapM_ setPartitionPtrs $ partitionMemory config
  comment "Temporarily set the interruptSource to yield."
  interruptSource <== Const 1
  comment "Jump to the kernel."
  call "risk_entry"

setPartitionPtrs :: (Name, PartitionMemory) -> RISK ()
setPartitionPtrs (name, PartitionMemory recv' send' _) = recvPtrs 0 recv'
  where
  recvPtrs :: Integer -> [(Integer, Name)] -> RISK ()
  recvPtrs index a = case a of
    [] -> recv index recv'
    (_, from) : rest -> do
      word64 (printf "%s_from_%s_head_index" name from) <== add (index    ) (word64 $ printf "%s_memory_ptr" name)
      word64 (printf "%s_from_%s_tail_index" name from) <== add (index + 1) (word64 $ printf "%s_memory_ptr" name)
      recvPtrs (index + 2) rest
  recv :: Integer -> [(Integer, Name)] -> RISK ()
  recv index a = case a of
    [] -> send index send'
    (s, from) : rest -> do
      word64 (printf "%s_from_%s_recv_buffer" name from) <== add index (word64 $ printf "%s_memory_ptr" name)
      recv (index + s) rest
  send :: Integer -> [(Integer, Name)] -> RISK ()
  send index a = case a of
    [] -> word64 (printf "%s_data" name) <== add index (word64 $ printf "%s_memory_ptr" name)
    (s, to) : rest -> do
      word64 (printf "%s_to_%s_send_buffer" name to) <== add index (word64 $ printf "%s_memory_ptr" name)
      send (index + s) rest
  add :: Integer -> E Word64 -> E Word64
  add i a = Add a $ Const $ fromIntegral i

-- Main kernel entry due to interrupt (timer, yield call, exception, etc).
declareKernelEntry :: RISK ()
declareKernelEntry = proc "risk_entry" $ do
  config <- config
  comment "Decrement the cycle counter.  Exit the simulator if zero."
  if' (cycleCount .== Const 0) (intrinsic ExitSimulation) (cycleCount <== Sub cycleCount (Const 1))
  comment "Save the context (stack pointer) of the active partition."
  onActivePartition (intrinsic . SaveContext) $ return ()
  comment "If the interrupt source is from a yielding partition, transfer messages from that partition."
  if' interruptSourceIsYield (onActivePartition transferMessages $ return ()) $ return ()
  comment "Switch context to the next partition in the schedule and run."
  case' schedulingPhase (map (scheduleCase config) $ sched config)
    (do
      let name = head $ schedule config
      comment "The default case starts the first partition in the schedule."
      schedulingPhase <== Const 0
      activePartition <== Const (fromIntegral $ partitionId config $ head $ schedule config)
      bool (name ++ "_initialized") <== true
      intrinsic $ RestoreContext name
      intrinsic $ StartPartition name)
  where
  sched :: Config -> [(Int, Int)]
  sched config = zip s $ tail s ++ [head s] where s = [0 .. length (schedule config) - 1]
  {-
  case' interruptSource
    [ (yield,     transferMessages)
    , (timer,     return ())
    , (io,        intrinsic IOInterruptHandler)
    , (exception, intrinsic PartitionExceptionHandler)
    ] Nothing
  runNextPartition
  where
  yield     = (.== Const 1)
  timer     = (.== Const 2)
  io        = (.== Const 3)
  exception = (.== Const 4)
  -}

scheduleCase :: Config -> (Int, Int) -> (E Word64 -> E Bool, RISK ())
scheduleCase config (curr, next) = ((.== (Const $ fromIntegral curr)), do
  comment "Set new scheduling phase and active partition."
  schedulingPhase <== Const (fromIntegral next)
  activePartition <== Const (fromIntegral $ partitionId config name)
  comment "Load the partition's stack pointer."
  intrinsic $ RestoreContext $ schedule config !! next
  if' partitionInitialized
      (do
        comment "If partition is already running, return from its previous call to risk_yield."
        intrinsic Return)
      (do
        comment "Else, initialize the partition by calling the partition's main entry point."
        partitionInitialized <== true
        intrinsic $ StartPartition name))
  where
  name = schedule config !! next
  partitionInitialized = bool (name ++ "_initialized")


-- Transfer messages from one partition to another.
transferMessages :: String -> RISK ()
transferMessages from = do
  spec <- spec
  sequence_ [ intrinsic $ TransferMessages sender sendSize receiver recvSize | Channel sender sendSize receiver recvSize  <- channels spec, sender == from ]
{-
  foreach sendBuffer
    sendPtr = sendBufferPtr
    headPtr = recvBufferHeadPtr
    tailPtr = recvBufferTailPtr
    while (messageAvailToSend && messageValid && spaceAvailInRecvBuffer)
      count  = 0
      length = sendMsgLength
      while (count < length)
        *tailPtr = *sendPtr
        sendPtr = sendPtr + 1
        tailPtr = (tailPtr + 1) & (log2 recvBufferSize - 1)
        count   = count + 1
-}

-- Do something based on the active partition.  TODO: Implement binary search for efficiency.
onActivePartition :: (String -> RISK ()) -> RISK () -> RISK ()
onActivePartition k def = do
  config <- config
  case' activePartition [ ((.== Const (fromIntegral i)), k name) | (name, i) <- zip (partitionNames config) [0 ..] ] def

