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

type RISK = GIGL Config Intrinsic

data Intrinsic
  = TransferMessages Int  -- ^ Transfer messages from a partition.
  | IOInterruptHandler
  | PartitionExceptionHandler
  | SaveContext
  | RunNextPartition
  | SetMemoryPtrs
  | InvalidExecution  -- ^ Marks the begining and end of code segements.  Used to check if execution goes off a cliff.
  deriving Show

-- | Builds the kernel from the kernel specification.
kernelProgram :: Spec -> Program Intrinsic
kernelProgram spec = snd $ elaborate (configure spec) kernel

-- Get the kernel configuration.
config :: RISK Config
config = getMeta

-- The kernel.
kernel :: RISK ()
kernel = do
  declareKernelInit
  declareKernelEntry

{-
  partitionInits      <- sequence [ var $ name ++ "_initialized" | name <- partitionNames config ] 
  partitionMemoryPtrs <- sequence [ var $ name ++ "_memory_ptr"  | name <- partitionNames config ] 
  partitionStackPtrs  <- sequence [ var $ name ++ "_stack_ptr"   | name <- partitionNames config ] 
  sequence_ [ var (name ++ "_data") | name <- partitionNames config ]
  sequence_ $ concat
    [ [ var (name ++ "_from_" ++ from ++ "_head_index")  | (_, from) <- recv ] ++
      [ var (name ++ "_from_" ++ from ++ "_tail_index")  | (_, from) <- recv ] ++
      [ var (name ++ "_from_" ++ from ++ "_recv_buffer") | (_, from) <- recv ] ++
      [ var (name ++ "_to_"   ++ to   ++ "_send_buffer") | (_, to)   <- send ]
    | (name, PartitionMemory recv send _) <- partitionMemory config
    ]
-}

interruptSource :: E Word64
interruptSource = Var "risk_interrupt_source"

activePartition :: E Word64
activePartition = Var "risk_active_partition"

schedulingPhase :: E Word64
schedulingPhase = Var "risk_scheduling_phase"

-- Kernel initialization.
declareKernelInit :: RISK ()
declareKernelInit = proc "risk_init" $ do
  config <- config
  comment "Initialize the active partition."
  activePartition <== Const 0xffffffffffffffff
  comment "Initialize the scheduling phase."
  schedulingPhase <== Const (fromIntegral $ length (schedule config) - 1)
  comment "Initialize the partition init flags."
  sequence_ [ bool (name ++ "_initialized")  <== false |  name <- partitionNames config ]
  comment "Initialize the partition memory pointers."
  intrinsic SetMemoryPtrs
  comment "Initialize the partition stack pointers."
  sequence_ [ word64 (name ++ "_stack_ptr") <== Add (word64 $ name ++ "_memory_ptr") (Const $ fromIntegral $ partitionMemorySize config name) | name <- partitionNames config ]
  comment "Initialize the partition channel buffer and data region pointers."
  mapM_ setPartitionPtrs $ partitionMemory config
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
      word64 (printf "%s_from_%s_tail_index" name from) <== add (index + 8) (word64 $ printf "%s_memory_ptr" name)
      recvPtrs (index + 16) rest
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
declareKernelEntry = proc "risk_entry_tmp" $ do
  saveContext
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

-- Saves the current context to tmp.
saveContext :: RISK ()
saveContext = intrinsic SaveContext

-- Transfer messages from current partition to recipients.
transferMessages :: RISK ()
transferMessages = onActivePartition $ intrinsic . TransferMessages
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
onActivePartition :: (Int -> RISK ()) -> RISK ()
onActivePartition k = do
  config <- config
  let f :: Int -> RISK ()
      f i = if i == totalPartitions config - 1 then k i else if' (activePartition .== Const (fromIntegral i)) (k i) (f $ i + 1)
  f 0

-- Runs the next partition in the schedule.
runNextPartition :: RISK ()
runNextPartition = do
  scheduleNextPartition
  intrinsic RunNextPartition

-- Schedules the next partition by updating schedulingPhase and activePartition.
scheduleNextPartition :: RISK ()
scheduleNextPartition = do
  config <- config
  let phases :: [(Word64, Word64)]  -- Maps schedule phase to parition id.
      phases = zip [0 ..] $ map (fromIntegral . partitionId config) $ schedule config
  schedulingPhase <== mux (schedulingPhase .== Const (fromIntegral $ length phases - 1)) (Const 0) (Add schedulingPhase $ Const 1)
  activePartition <== mux' [ (schedulingPhase .== Const phase, Const partition) | (phase, partition) <- phases ] $ Const 0

