-- | Kernel verification.
module RISK.Verify
  ( verifyKernel
  ) where

import Language.GIGL
import MonadLib
import System.IO
import Text.Printf

import RISK.Config
import RISK.Kernel
import RISK.Spec

-- | Verifies properties of the kernel.
verifyKernel :: Spec -> IO ()
verifyKernel spec = runVerification $ do
  --verify "termination"              $ termination                              program
  verify "scheduling phase updates" $ schedulingPhaseUpdates (schedule config) program
  verify "partition scheduling"     $ partitionScheduling    (schedule config) program
  where
  config   = configure spec
  --sched    = schedule config
  program  = validateProgram $ kernelProgram spec

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

validateProgram :: Program Intrinsic -> Program Intrinsic
validateProgram = id
{- XXX
  - All labels unique.
  - All variables unique.
  - All goto label references valid.
  - All variable references valid.
  - All assignments are valid: var or array index LHS.
-}

-- All kernel entry points will terminate and return control to a user partition (RunNextPartition).
{-
termination :: Program Intrinsic -> IO Bool
termination (Program _ procs _) = do
  when (not loopFree) $ do
    putStrLn "Program has loops:" 
    mapM_ (putStrLn . intercalate " -> ") loops

  when (not noInvalidTerminations) $ do
    putStrLn "Program has invalid terminations:" 
    mapM_ putStrLn invalidTerms

  return $ loopFree && noInvalidTerminations
  where
  labels = l stmt
  l a = case a of
    Label a -> [a]
    Seq a b -> l a ++ l b
    If  _ a b -> l a ++ l b
    _ -> []

  (_, transitions', invalidTerms') = trans ([], [], []) stmt
  transitions  = nub transitions'
  invalidTerms = nub invalidTerms'

  noInvalidTerminations = null invalidTerms

  loopFree = null loops

  loops :: [[String]]
  loops = concat [ [ p | p <- paths [] l, p /= nub p ] | l <- labels ]

  paths :: [String] -> String -> [[String]]
  paths sofar a
    | elem a sofar = [sofar']
    | otherwise    = concatMap (paths sofar') [ t | (f, t) <- transitions, f == a ]
    where
    sofar' = sofar ++ [a]

  trans :: ([String], [(String, String)], [String]) -> Stmt Intrinsic -> ([String], [(String, String)], [String])
  trans i@(from, sofar, invalidTerm) b = case b of
    Null -> i
    Assign _ _ -> i
    Label a -> ([a], [ (b, a) | b <- from ] ++ sofar, invalidTerm)
    Goto  a -> ([],  [ (b, a) | b <- from ] ++ sofar, invalidTerm)
    Seq a b -> trans (trans i a) b
    If _ a b -> (from', sofar', invalidTerm')
      where
      (fromA, sofarA, invalidTermA) = trans (from, [], []) a
      (fromB, sofarB, invalidTermB) = trans (from, [], []) b
      from'  = fromA ++ fromB
      sofar' = sofar ++ sofarA ++ sofarB
      invalidTerm' = invalidTerm ++ invalidTermA ++ invalidTermB
    Intrinsic RunNextPartition -> ([], sofar, invalidTerm)
    Intrinsic InvalidExecution -> ([], sofar, from ++ invalidTerm)
    Intrinsic _ -> i
-}

-- Verifies that the schedulingPhase variable is updated correctly.
schedulingPhaseUpdates :: [Name] -> Program Intrinsic -> IO Bool
schedulingPhaseUpdates _ _ = return False

-- Verifies that the correct activePartition corresponds with the schedulingPhase.
partitionScheduling :: [Name] -> Program Intrinsic -> IO Bool
partitionScheduling _ _ = return False

