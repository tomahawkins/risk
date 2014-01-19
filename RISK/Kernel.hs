module RISK.Kernel
  ( Intrinsic (..)
  , kernelProgram
  ) where

import Data.Word
import Language.GIGL

import RISK.Config
import RISK.Spec

type RISK = GIGL (Config, KernelState) Intrinsic

data Intrinsic
  = TransferMessages Int  -- ^ Transfer messages from a partition.
  | KernelInit
  | IOInterruptHandler
  | PartitionExceptionHandler
  | SaveContext
  | RunNextPartition
  | InvalidExecution  -- ^ Marks the begining and end of code segements.  Used to check if execution goes off a cliff.
  deriving Show

-- | Builds the kernel from the kernel specification.
kernelProgram :: Spec -> Program Intrinsic
kernelProgram spec = snd $ elaborate (configure spec, error "KernelState not defined.") $ invalidExecution >> kernel >> invalidExecution

invalidExecution :: RISK ()
invalidExecution = intrinsic InvalidExecution

-- Get the kernel configuration.
config :: RISK Config
config = getMeta >>= return . fst

-- The kernel.
kernel :: RISK ()
kernel = do
  declareKernelState
  declareKernelInit
  declareKernelEntry
  declareRunNextPartition

-- Declares a code segement.
block :: String -> RISK () -> RISK ()
block name code = do
  invalidExecution
  label name code
  invalidExecution

-- All the kernel state variables.
data KernelState = KernelState
  { interruptSource' :: E Word64
  , activePartition' :: E Word64
  , schedulingPhase' :: E Word64
  }

-- Build up the kernel state (global) variables and put it into the RISK monad.
declareKernelState :: RISK ()
declareKernelState = do
  config <- config
  interruptSource <- var "interruptSource" Nothing
  activePartition <- var "activePartition" Nothing
  schedulingPhase <- var "schedulingPhase" Nothing
  setMeta (config, KernelState
    { interruptSource' = interruptSource
    , activePartition' = activePartition
    , schedulingPhase' = schedulingPhase
    })

-- Get a kernel state field.
kernelState :: (KernelState -> a) -> RISK a
kernelState f = getMeta >>= return . f . snd

-- Various kernal state getters.
interruptSource = kernelState interruptSource'
activePartition = kernelState activePartition'
schedulingPhase = kernelState schedulingPhase'

-- Kernel initialization.
declareKernelInit :: RISK ()
declareKernelInit = block "kernelInit" $ do
  intrinsic KernelInit
  runNextPartition

-- Main kernel entry due to interrupt (timer, yield call, exception, etc).
declareKernelEntry :: RISK ()
declareKernelEntry = block "kernelEntry" $ do
  saveContext
  i <- interruptSource
  case' i
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
  p <- activePartition
  let f :: Int -> RISK ()
      f i = if i == totalPartitions config - 1 then k i else if' (p .== Const (fromIntegral i)) (k i) (f $ i + 1)
  f 0

-- Runs the next partition in the schedule.
runNextPartition :: RISK ()
runNextPartition = goto "runNextPartition"

-- Runs the next partition in the schedule.
declareRunNextPartition :: RISK ()
declareRunNextPartition = block "runNextPartition" $ do
  scheduleNextPartition
  intrinsic RunNextPartition

-- Schedules the next partition by updating schedulingPhase and activePartition.
scheduleNextPartition :: RISK ()
scheduleNextPartition = do
  config <- config
  let phases :: [(Word64, Word64)]  -- Maps schedule phase to parition id.
      phases = zip [0 ..] $ map (fromIntegral . partitionId config) $ schedule config
  schedulingPhase <- schedulingPhase
  activePartition <- activePartition
  schedulingPhase <== mux (schedulingPhase .== Const (fromIntegral $ length phases - 1)) (Const 0) (Add schedulingPhase $ Const 1)
  activePartition <== mux' [ (schedulingPhase .== Const phase, Const partition) | (phase, partition) <- phases ] $ Const 0

