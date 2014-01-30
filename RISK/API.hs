-- | Generates the API for a configured kernel.
module RISK.API
  ( generateAPI
  ) where

import Text.Printf

import RISK.Config
import RISK.Spec

-- | Generates the API files for a configured RISK kernel.
generateAPI :: Spec -> IO ()
generateAPI spec = sequence_
  [ do writeFile ("risk_api_" ++ name ++ ".h") $ headerFile name memory
       writeFile ("risk_api_" ++ name ++ ".c") $ cFile      name memory
  | (name, memory) <- partitionMemory $ configure spec
  ]

headerFile :: Name -> PartitionMemory -> String
headerFile name memory = unlines
  [ "// RISK Partition " ++ name
  , ""
  , "#ifdef __cplusplus"
  , "extern \"C\" {"
  , "#endif"
  , ""
  , "#include \"risk_lib.h\""
  , ""
  , "// The main entry point for the partition."
  , printf "void %s_main(void);" name
  , ""
  , "// Yield control back to kernel."
  , "void risk_yield(void);"
  , ""
  , "// Message reception on incoming channels.  If no messages are available on a channel, size will be zero."
  , unlines [ printf "void %s_from_%s_recv_msg(word * size, word * msg);" name sender | (_, sender) <- recvBuffers memory ]
  , "// Message transmission on outgoing channels."
  , unlines [ printf "void %s_to_%s_send_msg(word size, word * msg);" name receiver | (_, receiver) <- sendBuffers memory ]
  , ""
  , "#ifdef __cplusplus"
  , "}"
  , "#endif"
  , ""
  ]

cFile :: Name -> PartitionMemory -> String
cFile name memory = unlines
  [ printf "// RISK Partition %s" name
  , printf ""
  , printf "#include \"risk_api_%s.h\"" name
  , printf ""
  , concatMap recvMessage $ recvBuffers memory
  , concatMap sendMessage $ sendBuffers memory
  , printf ""
  ]
  where
  recvMessage :: (Integer, Name) -> String
  recvMessage (size, sender) = unlines
    [ printf "// Receive buffer from %s." sender
    , printf "extern word const * const %s_from_%s_recv_buffer;  // %d bytes" name sender size
    , printf ""
    , printf "// Head and tail indecies of receive buffer from %s.  Head is managed by the partition.  Tail is managed by the kernel." sender
    , printf "extern word * const %s_from_%s_recv_head_index;" name sender
    , printf "extern word const * const %s_from_%s_recv_tail_index;" name sender
    , printf ""
    , printf "// Receives a message from the %s partition.  If no messages are available, size will be zero." sender
    , printf "void %s_from_%s_recv_msg(word * size, word * msg)" name sender
    , printf "{"
    , printf "\t//XXX"
    , printf "}"
    , printf ""
    ]
  sendMessage :: (Integer, Name) -> String
  sendMessage (size, receiver) = unlines
    [ printf "// Sending buffer to %s." receiver
    , printf "extern word * const %s_to_%s_send_buffer;  // %d words" name receiver size
    , printf ""
    , printf "// Sends a message to the %s partition." receiver
    , printf "void %s_to_%s_send_msg(word size, word * msg)" name receiver
    , printf "{"
    , printf "\t//XXX"
    , printf "}"
    , printf ""
    ]


