module RISK.Kernel
  ( kernelProgram
  ) where

import Data.Word
import Language.GIGL

import RISK.Config
import RISK.Spec

type RISK = GIGL (Config, KernelState)

data KernelState = KernelState
  { interrupt :: E Word64
  }

-- | Builds the kernel from the kernel specification.
kernelProgram :: Spec -> Program
kernelProgram spec = snd $ elaborate (configure spec, error "KernelState not defined.") kernel

-- | The kernel
kernel :: RISK ()
kernel = do
  declareKernelState
  declareKernelInit
  declareKernelEntry
  declareNextThread

-- | Build up the kernel state variable and put it into the RISK monad.
declareKernelState :: RISK ()
declareKernelState = do
  config <- kernelConfig
  setMeta (config, KernelState
    { interrupt = undefined
    })

-- | Get the kernel configuration.
kernelConfig :: RISK Config
kernelConfig = getMeta >>= return . fst

-- | Get the kernel state.
kernelState :: RISK KernelState
kernelState = getMeta >>= return . snd

-- | Kernel initialization.
declareKernelInit :: RISK ()
declareKernelInit = function "kernelInit" $ do
  undefined

-- | Main kernel entry due to interrupt (timer, yield call, exception, etc).
declareKernelEntry :: RISK ()
declareKernelEntry = function "kernelEntry" $ do
  saveContext
  s <- kernelState
  case' (interrupt s)
    [ (yield, transferMessages >> goto "nextThread")
    , (timer,                     goto "nextThread")
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

-- | Saves the current contents to tmp.
saveContext :: RISK ()
saveContext = undefined

-- | Transfer messages from current thread.
transferMessages :: RISK ()
transferMessages = undefined

-- | Runs the next thread in the schedule.
declareNextThread :: RISK ()
declareNextThread = function "nextThread" $ do
  undefined

