-- | Generates the API for a configured kernel.
module RISK.API
  ( generateAPI
  ) where

import Text.Printf

import RISK.Compile (indent)
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
  [ "// RISK API for the " ++ name ++ " Partition"
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
  , "// Receive message on incoming channels.  If no messages are available on a channel, size will be zero."
  , unlines [ printf "void %s_from_%s_recv_msg(word * size, word * msg);" name sender | (_, sender) <- recvBuffers memory ]
  , "// Transmit message on outgoing channels."
  , unlines [ printf "void %s_to_%s_send_msg(word size, word * msg);" name receiver | (_, receiver) <- sendBuffers memory ]
  , ""
  , "#ifdef __cplusplus"
  , "}"
  , "#endif"
  , ""
  ]

cFile :: Name -> PartitionMemory -> String
cFile name memory = unlines
  [ "// RISK API for the " ++ name ++ " Partition"
  , printf ""
  , printf "#include \"risk_api_%s.h\"" name
  , printf ""
  , concatMap recvMessage $ recvBuffers memory
  , concatMap sendMessage $ sendBuffers memory
  , printf ""
  ]
  where
  recvMessage :: (Int, Name) -> String
  recvMessage (size, sender) = unlines
    [ printf "// Receive buffer from %s." sender
    , printf "extern word const * const %s_recv_buffer;  // 0x%x words" prefix (2 ^ size :: Int)
    , printf ""
    , printf "// Head and tail indecies of receive buffer from %s.  Head is managed by the partition.  Tail is managed by the kernel." sender
    , printf "extern word * const %s_head_index;" prefix
    , printf "extern word const * const %s_tail_index;" prefix
    , printf ""
    , printf "// Receives a message from the %s partition.  If no messages are available, size will be zero." sender
    , printf "void %s_recv_msg(word * size, word * msg)" prefix
    , "{"
    , indent $ unlines
      [ "word i;"
      , printf "if (*%s_head_index == *%s_tail_index)" prefix prefix
      , indent "*size = 0;"
      , printf "else {"
      , indent $ unlines
        [ printf "*size = %s_recv_buffer[*%s_head_index & 0x%x];" prefix prefix mask
        , printf "*%s_head_index = *%s_head_index + 1;" prefix prefix
        , printf "for (i = 0; i < *size; i++) {"
        , indent $ unlines
          [ printf "msg[i] = %s_recv_buffer[*%s_head_index & 0x%x];" prefix prefix mask
          , printf "*%s_head_index = *%s_head_index + 1;" prefix prefix
          ]
        , "}"
        ] 
      , "}"
      ]
    , "}"
    , ""
    ]
    where
    prefix :: String
    prefix = printf "%s_from_%s" name sender
    mask :: Int
    mask = 2 ^ size - 1

  sendMessage :: (Int, Name) -> String
  sendMessage (size, receiver) = unlines
    [ printf "// Sending buffer to %s." receiver
    , printf "extern word * const %s_to_%s_send_buffer;  // 0x%x words" name receiver (2 ^ size :: Int)
    , printf ""
    , printf "// Sends a message to the %s partition." receiver
    , printf "void %s_to_%s_send_msg(word size, word * msg)" name receiver
    , printf "{"
    , printf "\t//XXX"
    , printf "}"
    , printf ""
    ]

