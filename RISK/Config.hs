-- | RISK configuration.
module RISK.Config
  ( Config          (..)
  , PartitionMemory (..)
  , configure
  ) where

import RISK.Spec

-- | Memory is a list of recv buffers, send buffers, and a general purpose memory region.
data PartitionMemory = PartitionMemory
  { recvBuffers :: [(Integer, Name)]  -- ^ A list of receiving buffers (size, corresponding channel name).
  , sendBuffers :: [(Integer, Name)]  -- ^ A list of sending buffers (size, corresponding channel name).
  , dataSize    :: Integer            -- ^ Size of general purpose memory region.
  }

-- | Kernel configuration.
data Config = Config
  { partitionMemory :: [(Name, PartitionMemory)]  -- ^ The memory layout of all the partitions.
  }

instance Show Config where
  show config = unlines $ map showP $ partitionMemory config
    where
    showP (name, PartitionMemory recv send size) = name ++ ":  " ++ show size ++ "  " ++ show recv ++ "  " ++ show send

-- | Generate a configuration given a kernel specification.
configure :: Spec -> Config
configure spec' = Config [ (name, partitionMemory name size) | Partition name _ size  <- partitions spec ]
  where
  spec = validateSpec spec'

  -- A partitions' memory is receive and send buffers followed by general purpose memory.
  partitionMemory :: Name -> Integer -> PartitionMemory
  partitionMemory name size = PartitionMemory recvBuffers sendBuffers $ size - (sum $ fst $ unzip $ recvBuffers ++ sendBuffers)
    where
    recvBuffers = [ (cReceiverBufferSize c, cSender   c) | c <- channels spec, cReceiver c == name ]
    sendBuffers = [ (cSenderBufferSize   c, cReceiver c) | c <- channels spec, cSender   c == name ]

