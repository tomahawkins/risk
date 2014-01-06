module RISK.Language
  ( Program (..)
  , E       (..)
  , kernelProgram
  ) where

import MonadLib hiding (Label)

type RISK = StateT (KernelState, Program) IO

data Program = Program

data E
  = Variable   String
  | ArrayIndex E E

data KernelState = KernelState
  { interrupt :: E
  }

type Label = String


-- | Declares a new state variable.
word :: String -> RISK E
word = undefined

-- | Declares a new state array variable.
array :: String -> Integer -> RISK E
array = undefined

-- | Declares a top level function.
--   Functions aren't really functions; they don't take arguments and they don't return results.
function :: Label -> RISK () -> RISK ()
function = undefined

-- | Jump to a label.
goto :: Label -> RISK ()
goto = undefined

-- | Case statement with no default.
case' :: E -> [(E -> E, RISK())] -> RISK ()
case' a b = case b of
  [] -> return ()
  (pred, stmt) : rest -> ite (pred a) stmt $ case' a rest

-- | If then else statement.
ite :: E -> RISK () -> RISK () -> RISK ()
ite = undefined




-- | Elaborates the kernel from the kernel description.
kernelProgram :: IO Program
kernelProgram = runStateT (error "KernelState not defined.", Program) kernel >>= return . snd . snd

-- | The kernel
kernel ::  RISK ()
kernel = do
  declareKernelState
  declareKernelInit
  declareKernelEntry
  declareNextThread

-- | Build up the kernel state variable and put it into the RISK monad.
declareKernelState :: RISK ()
declareKernelState = do
  (_, program) <- get
  set (KernelState
    { interrupt = undefined
    }, program)

-- | Get the kernel state.
kernelState :: RISK KernelState
kernelState = get >>= return . fst

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

