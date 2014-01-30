{-# LANGUAGE GADTs #-}
module RISK.Compile
  ( compile
  , indent
  ) where

import Data.List (intercalate)
import Language.GIGL
import Text.Printf

import RISK.Kernel
import RISK.Spec

-- | Compiles a configured RISK kernel to C.
compile :: Spec -> String
compile spec = unlines $ 
  [ printf "void %s (void);" name | (name, _) <- procs ] ++
  map compileProc procs
  where
  procs = kernelProgram spec

compileProc :: (Name, Stmt Intrinsic) -> String
compileProc (name, stmt) = unlines
  [ printf "void %s(void)" name
  , "{"
  , indent $ compileStmt stmt
  , "}"
  , ""
  ]

compileStmt :: Stmt Intrinsic -> String
compileStmt a = case a of
  Comment a -> "// " ++ a ++ "\n"
  Null -> ""
  Seq a b -> compileStmt a ++ compileStmt b
  If a b c -> unlines
    [ printf "if (%s) {" $ compileExpr a
    , compileStmt b
    , "}"
    , "else {"
    , compileStmt c
    , "}"
    ]
  Assign (Var a) b -> printf "%s = %s;\n" a $ compileExpr b
  Assign _ _ -> error "Invalid LHS in assignment."
  Call a -> printf "%s();\n" a
  Intrinsic a -> case a of
    SetMemoryPtrs       -> "risk_set_memory_ptrs();\n"
    ExitSimulation      -> "exit(0);\n"
    Return              -> "return;\n"
    SaveContext    name -> printf "asm(\"movq %%%%rsp, %%0\" : \"=r\" (%s_stack_ptr) : );\n" name
    RestoreContext name -> printf "asm(\"movq %%0, %%%%rsp\" : : \"r\" (%s_stack_ptr));\n" name
    StartPartition name -> printf "%s_main();\n" name
    TransferMessages from sendSize to recvSize -> printf "risk_transfer_messages(%s);\n" $ intercalate ", " args
      where
      args :: [String]
      args =
        [ printf "%d" sendSize
        , printf "(word *) %s_to_%s_send_buffer" from to
        , printf "%d" recvSize
        , printf "(word *) %s_from_%s_recv_buffer" to from
        , printf "%s_from_%s_head_index" to from
        , printf "& %s_from_%s_tail_index" to from
        ]

compileExpr :: E a -> String
compileExpr a = case a of
  Var     a     -> a
  Index   _ _   -> error "Array Index not supported."
  Let     _ _ _ -> error "Let expressions not supported."
  Untyped a     -> f a
  Pair    _ _   -> error "Pair expressions not supported."
  Fst     _     -> error "Fst expressions not supported."
  Snd     _     -> error "Snd expressions not supported."
  Const   a     -> cValue $ value a
  Add     a b   -> f2 "+" a b
  Sub     a b   -> f2 "-" a b
  Not     a     -> f1 "!" a
  And     a b   -> f2 "&&" a b
  Or      a b   -> f2 "||" a b
  Imply   a b   -> printf "(! %s || %s)" (f a) (f b)
  Equiv   a b   -> f2 "==" a b
  Eq      a b   -> f2 "==" a b
  Mux     a b c -> printf "(%s ? %s : %s)" (f a) (f b) (f c)
  where
  f :: E a -> String
  f = compileExpr
  f1 :: String -> E a -> String
  f1 op a = "(" ++ op ++ " " ++ f a ++ ")"
  f2 :: String -> E a -> E a -> String
  f2 op a b = "(" ++ f a ++ " " ++ op ++ " " ++ f b ++ ")"
  cValue :: Value -> String
  cValue a = case a of
    VBool   True  -> "1"
    VBool   False -> "0"
    VWord64 a     -> printf "0x%016xULL" a
    VPair   _ _   -> error "Pair values not supported."

indent :: String -> String
indent = intercalate "\n" . map ("\t" ++) . lines

