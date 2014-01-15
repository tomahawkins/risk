module RISK.Kernel
  ( kernelProgram
  ) where

import Data.Word
import Language.GIGL

import RISK.Config
import RISK.Spec

type RISK = GIGL (Config, KernelState)

-- | Builds the kernel from the kernel specification.
kernelProgram :: Spec -> Program
kernelProgram spec = snd $ elaborate (configure spec, error "KernelState not defined.") kernel

-- | Get the kernel configuration.
kernelConfig :: RISK Config
kernelConfig = getMeta >>= return . fst

-- | The kernel
kernel :: RISK ()
kernel = do
  declareKernelState
  declareKernelInit
  declareKernelEntry
  declareRunNextPartition

-- | All the kernel state variables.
data KernelState = KernelState
  { interruptSource' :: E Word64
  , activePartition' :: E Word64
  }

-- | Build up the kernel state (global) variables and put it into the RISK monad.
declareKernelState :: RISK ()
declareKernelState = do
  config <- kernelConfig
  setMeta (config, KernelState
    { interruptSource' = undefined
    , activePartition' = undefined
    })

-- | Get a kernel state field.
kernelState :: (KernelState -> a) -> RISK a
kernelState f = getMeta >>= return . f . snd

-- Various kernal state getters.
interruptSource = kernelState interruptSource'
activePartition = kernelState activePartition'


-- | Kernel initialization.
declareKernelInit :: RISK ()
declareKernelInit = function "kernelInit" $ do
  undefined

-- | Main kernel entry due to interrupt (timer, yield call, exception, etc).
declareKernelEntry :: RISK ()
declareKernelEntry = function "kernelEntry" $ do
  saveContext
  i <- interruptSource
  case' i
    [ (yield, transferMessages >> runNextPartition)
    , (timer,                     runNextPartition)
    , (io,        undefined)
    , (pageFault, undefined)
    , (exception, undefined)
    ]
  where
  yield     = undefined
  timer     = undefined
  io        = undefined
  pageFault = undefined
  exception = undefined

-- | Saves the current context to tmp.
saveContext :: RISK ()
saveContext = undefined

-- | Transfer messages from current partition to recipients.
transferMessages :: RISK ()
transferMessages = undefined

-- | Runs the next partition in the schedule.
declareRunNextPartition :: RISK ()
declareRunNextPartition = function "runNextPartition" $ do
  undefined

runNextPartition :: RISK ()
runNextPartition = goto "runNextPartition"

