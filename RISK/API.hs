-- | Generates the API for a configured kernel.
module RISK.API
  ( generateAPI
  , word
  , byte
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

word :: String
word = "unsigned long long"

byte :: String
byte = "unsigned char"

headerFile :: Name -> PartitionMemory -> String
headerFile name memory = unlines
  [ printf "// RISK Partition %s" name
  , printf ""
  , printf "// Yield control back to kernel."
  , printf "void risk_yield(void);"
  , printf ""
  , printf "// Message reception on incoming channels.  If no messages are available on a channel, size will be zero."
  , unlines [ printf "void %s_from_%s_recv_msg(%s * size, %s * msg);" name sender word byte | (_, sender) <- recvBuffers memory ]
  , printf "// Message transmission on outgoing channels."
  , unlines [ printf "void %s_to_%s_send_msg(%s size, %s * msg);" name receiver word byte | (_, receiver) <- sendBuffers memory ]
  , printf ""
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
    , printf "extern %s const * const %s_from_%s_recv_buffer;  // %d bytes" byte name sender size
    , printf ""
    , printf "// Head and tail indecies of receive buffer from %s.  Head is managed by the partition.  Tail is managed by the kernel." sender
    , printf "extern %s       * const %s_from_%s_recv_head_index;" word name sender
    , printf "extern %s const * const %s_from_%s_recv_tail_index;" word name sender
    , printf ""
    , printf "// Receives a message from the %s partition.  If no messages are available, size will be zero." sender
    , printf "void %s_from_%s_recv_msg(%s * size, %s * msg)" name sender word byte
    , printf "{"
    , printf "\t//XXX"
    , printf "}"
    , printf ""
    ]
  sendMessage :: (Integer, Name) -> String
  sendMessage (size, receiver) = unlines
    [ printf "// Sending buffer to %s." receiver
    , printf "extern %s * const %s_to_%s_send_buffer;  // %d bytes" byte name receiver size
    , printf ""
    , printf "// Sends a message to the %s partition." receiver
    , printf "void %s_to_%s_send_msg(%s size, %s * msg)" name receiver word byte
    , printf "{"
    , printf "\t//XXX"
    , printf "}"
    , printf ""
    ]


