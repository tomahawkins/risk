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
  [ do writeFile ("risk_partition_" ++ name ++ ".h") $ headerFile name memory
       writeFile ("risk_partition_" ++ name ++ ".c") $ cFile      name memory
  | (name, memory) <- partitionMemory $ configure spec
  ]

word = "unsigned long long"
byte = "unsigned char"

headerFile :: Name -> PartitionMemory -> String
headerFile name memory = unlines
  [ printf "// RISK Partition %s" name
  , printf ""
  , printf "// Yields control back to kernel."
  , printf "void risk_partition_%s_yield(void);" name
  , printf ""
  , printf ""
  , concatMap recvMessage $ recvBuffers memory
  , concatMap sendMessage $ sendBuffers memory
  , printf ""
  ]
  where
  recvMessage :: (Integer, Name) -> String
  recvMessage (_, sender) = unlines
    [ printf "// Receives a message from the %s partition.  If no messages are available, size will be zero." sender
    , printf "void risk_partition_%s_recv_msg_from_%s(%s * size, %s * msg);" name sender word byte
    , printf ""
    ]
  sendMessage :: (Integer, Name) -> String
  sendMessage (_, receiver) = unlines
    [ printf "// Sends a message to the %s partition." receiver
    , printf "void risk_partition_%s_send_msg_to_%s(%s size, %s * msg);" name receiver word byte
    , printf ""
    ]

cFile :: Name -> PartitionMemory -> String
cFile name memory = unlines
  [ printf "// RISK Partition %s" name
  , printf ""
  , printf "#include \"risk_partition_%s.h\"" name
  , printf ""
  , printf "// Yields control back to the kernel."
  , printf "void risk_partition_%s_yield(void)" name
  , printf "{"
  , printf "\t//XXX"
  , printf "}"
  , printf ""
  , printf ""
  , concatMap recvMessage $ recvBuffers memory
  , concatMap sendMessage $ sendBuffers memory
  , printf ""
  ]
  where
  recvMessage :: (Integer, Name) -> String
  recvMessage (size, sender) = unlines
    [ printf "// Receive buffer from %s." sender
    , printf "extern %s risk_partition_%s_recv_buffer_from_%s[%d];" byte name sender size
    , printf ""
    , printf "// Head and tail indecies of receive buffer from %s.  Head is managed by the partition.  Tail is managed by the kernel." sender
    , printf "extern %s       * const risk_partition_%s_recv_head_index_from_%s;" word name sender
    , printf "extern %s const * const risk_partition_%s_recv_tail_index_from_%s;" word name sender
    , printf ""
    , printf "// Receives a message from the %s partition.  If no messages are available, size will be zero." sender
    , printf "void risk_partition_%s_recv_msg_from_%s(%s * size, %s * msg)" name sender word byte
    , printf "{"
    , printf "\t//XXX"
    , printf "}"
    , printf ""
    ]
  sendMessage :: (Integer, Name) -> String
  sendMessage (size, receiver) = unlines
    [ printf "// Sending buffer to %s." receiver
    , printf "extern %s risk_partition_%s_send_buffer_to_%s[%d];" byte name receiver size
    , printf ""
    , printf "// Sends a message to the %s partition." receiver
    , printf "void risk_partition_%s_send_msg_to_%s(%s size, %s * msg)" name receiver word byte
    , printf "{"
    , printf "\t//XXX"
    , printf "}"
    , printf ""
    ]


