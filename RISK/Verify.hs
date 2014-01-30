-- | Kernel verification.
module RISK.Verify
  ( verifyKernel
  ) where

import Data.List (nub)
import Data.Maybe (fromJust)
import Language.GIGL
import MonadLib
import System.IO
import Text.Printf

--import RISK.Config
import RISK.Kernel
import RISK.Spec

-- | Verifies properties of the kernel.
verifyKernel :: Spec -> IO ()
verifyKernel spec = runVerification $ do
  verify "kernel always terminates"                $ termination       program
  verify "kernel always returns to user partition" $ returnToPartition program
  verify "kernel only transfers messages between specified partitions" $ validMessageTransfer spec program
  --verify "scheduling phase updates" $ schedulingPhaseUpdates (schedule config) program
  --verify "partition scheduling"     $ partitionScheduling    (schedule config) program
  --verify "partition context initialization"     $ XXX  -- All stack pointers initialized correctly.
  where
  --config  = configure spec
  program = kernelProgram spec

type Verify = StateT [(String, IO Bool)] Id

verify :: String -> IO Bool -> Verify ()
verify name f = get >>= set . (++ [(name, f)])

runVerification :: Verify () -> IO ()
runVerification suite = do
  results <- mapM (\ (name, f) -> do
    printf "Verifying %s ...\n" name
    hFlush stdout
    result <- f
    if result then return () else do
      putStrLn $ "FAIL: " ++ name
      hFlush stdout
    return result) checks
  putStrLn $ if and results then "Kernel verified." else "Kernel verification FAILED."
  where
  checks :: [(String, IO Bool)]
  checks = snd $ runId $ runStateT [] suite

-- All kernel entry points will terminate.  Kernel will not get stuck in an infinite loop.
termination :: Program Intrinsic -> IO Bool
termination program 
  | callGraphCorrect = return True
  | otherwise = do
    putStrLn "Unexpected call graph:" 
    sequence_ [ printf "  %s -> %s\n" a b | (a, b) <- callGraph ]
    return False
  where
  calls :: Stmt Intrinsic -> [String]
  calls a = case a of
    Comment _ -> []
    Null -> []
    Seq a b -> calls a ++ calls b
    If _ a b -> calls a ++ calls b
    Assign _ _ -> []
    Call a -> [a]
    Intrinsic _ -> []

  expectedCallGraph = [("risk_init", "risk_entry")]
  callGraph = nub [ (caller, callee) | (caller, stmt) <- program, callee <- calls stmt ]
  callGraphCorrect = expectedCallGraph == callGraph

-- The risk_entry procedure will always return to a user partition.
returnToPartition :: Program Intrinsic -> IO Bool
returnToPartition program
  | null invalidPaths = return True
  | otherwise = do
    putStrLn "Invalid paths in risk_entry:"
    mapM_ print invalidPaths
    return False
  where
  entryPaths = paths $ fromJust $ lookup "risk_entry" program
  invalidPaths :: [Path]
  invalidPaths = filter (not . validPath) entryPaths
  validPath :: Path -> Bool
  validPath a = restoresContext a -- && 
  restoresContext :: Path -> Bool
  restoresContext = any restoreContext
  restoreContext a = case a of
    Intrinsic' (RestoreContext _) -> True
    _ -> False

data Step
  = Call'      String
  | Intrinsic' Intrinsic
  deriving Show

type Path = [Step]

-- All possible paths through a statement.  TODO: Filter out impossible paths (some simple variable value analysis).
paths :: Stmt Intrinsic -> [Path]
paths a = case a of
  Seq       a b   -> [ a ++ b | a <- paths a, b <- paths b ]
  If        _ a b -> paths a ++ paths b
  Comment   _     -> [[]]
  Null            -> [[]]
  Assign    _ _   -> [[]]
  Call      a     -> [[Call'      a]]
  Intrinsic a     -> [[Intrinsic' a]]

-- Only messages are transfered between specified partitions.
validMessageTransfer :: Spec -> Program Intrinsic -> IO Bool
validMessageTransfer spec program
  | null invalidTransfers = return True
  | otherwise = do
    putStrLn "Invalid message transfers:"
    mapM_ print invalidTransfers
    return False
  where
  transfers = [ (a, b) | TransferMessages a _ b _ <- programIntrinsics program ]
  validTransfers = [ (a, b) | Channel a _ b _ <- channels spec ]
  invalidTransfers = filter (not . flip elem validTransfers) transfers

programIntrinsics :: Program Intrinsic -> [Intrinsic]
programIntrinsics p = nub $ f $ foldl1 Seq $ snd $ unzip p
  where
  f :: Stmt Intrinsic -> [Intrinsic]
  f a = case a of
    Seq       a b   -> f a ++ f b
    If        _ a b -> f a ++ f b
    Comment   _     -> []
    Null            -> []
    Assign    _ _   -> []
    Call      _     -> []
    Intrinsic a     -> [a]


{-
-- Verifies that the schedulingPhase variable is updated correctly.
schedulingPhaseUpdates :: [Name] -> Program Intrinsic -> IO Bool
schedulingPhaseUpdates _ _ = return False

-- Verifies that the correct activePartition corresponds with the schedulingPhase.
partitionScheduling :: [Name] -> Program Intrinsic -> IO Bool
partitionScheduling _ _ = return False
-}
