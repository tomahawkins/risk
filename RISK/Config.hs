-- | RISK configuration.
module RISK.Config
  ( Config          (..)
  , PartitionMemory (..)
  , configure
  , partitionId
  , partitionNames
  , totalPartitions
  , partitionMemorySize
  ) where

import Data.Maybe (fromJust)
import Data.List  (elemIndex)

import RISK.Spec

-- | Memory is a list of recv buffers, send buffers, and a general purpose memory region.
data PartitionMemory = PartitionMemory
  { recvBuffers :: [(Int, Name)]  -- ^ A list of receiving buffers (log2 size, corresponding channel name).
  , sendBuffers :: [(Int, Name)]  -- ^ A list of sending buffers (log2 size, corresponding channel name).
  , dataSize    :: Int            -- ^ Size of general purpose memory region.
  }

-- | Kernel configuration.
data Config = Config
  { partitionMemory :: [(Name, PartitionMemory)]  -- ^ The memory layout of all the partitions.
  , schedule        :: [Name]                     -- ^ Roundrobin schedule of partitions.
  }

instance Show Config where
  show config = unlines $ map showP $ partitionMemory config
    where
    showP (name, PartitionMemory recv send size) = name ++ ":  " ++ show size ++ "  " ++ show recv ++ "  " ++ show send

-- | Generate a configuration given a kernel specification.
configure :: Spec -> Config
configure spec' = Config
  { partitionMemory = [ (name, partitionMemory name size) | Partition name _ size  <- partitions spec ]
  , schedule        = [ name | Partition name _ _ <- partitions spec ]
  }
  where
  spec = validateSpec spec'

  -- A partitions' memory is receive and send buffers followed by general purpose memory.
  partitionMemory :: Name -> Int -> PartitionMemory
  partitionMemory name size = PartitionMemory recvBuffers sendBuffers $ size - (length recvBuffers * 2 + (sum $ fst $ unzip $ recvBuffers ++ sendBuffers))
    where
    recvBuffers = [ (cReceiverBufferSize c, cSender   c) | c <- channels spec, cReceiver c == name ]
    sendBuffers = [ (cSenderBufferSize   c, cReceiver c) | c <- channels spec, cSender   c == name ]

-- | Partition names.
partitionNames :: Config -> [Name]
partitionNames = fst .unzip . partitionMemory

-- Partition memory size in bytes.
partitionMemorySize :: Config -> Name -> Int
partitionMemorySize config name = case a of
  [] -> error $ "No partition named: " ++ name
  [a] -> a
  _ -> error $ "Multiple partitions named: " ++ name
  where
  a = [ length recv * 2 + sum (fst $ unzip $ recv ++ send) + dat | (name', PartitionMemory recv send dat) <- partitionMemory config, name == name' ]
  

-- | Total number of partitions.
totalPartitions :: Config -> Int
totalPartitions = length . partitionNames

-- | Partition id.
partitionId :: Config -> Name -> Int
partitionId config name = fromJust $ elemIndex name $ partitionNames config

