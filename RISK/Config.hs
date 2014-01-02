-- | RISK configuration.
module RISK.Config
  ( Config             (..)
  , Partition          (..)
  , Channel            (..)
  , ScheduleConstraint (..)
  , Name
  , graphviz
  ) where

import Text.Printf

-- | Partition names.
type Name = String

-- | Overal kernel configuration.
data Config = Config
  { partitions :: [Partition]              -- ^ The list of partitions.
  , channels   :: [Channel]                -- ^ The partition communication channels (sender, receiver, buffer size).
  , scheduling :: [ScheduleConstraint]     -- ^ Partition scheduling constraints.
  }

-- | Partition configuration.
data Partition = Partition
  { pName   :: Name         -- ^ The partition name.
  , pRate   :: Maybe Double -- ^ Execution rate of partition in Hz.  If Nothing, thread runs in background.
  , pMemory :: Integer      -- ^ The memory size allocated to the partition in bytes.
  }

-- | Inter partition communication channel.
data Channel = Channel
  { cSender             :: Name       -- ^ Sending partition name.
  , cSenderBufferSize   :: Integer    -- ^ Sending buffer size in bytes.
  , cReceiver           :: Name       -- ^ Receiving partition name.
  , cReceiverBufferSize :: Integer    -- ^ Receiving buffer size in bytes.
  }

-- | Partition scheduling constraints.
data ScheduleConstraint
  -- | Execution priority between two partitions.  Overrides scheduling by channels if one exists.
  = Before Name Name

-- | Generate a Graphviz diagram of a kernel configuration.
graphviz :: Config -> String
graphviz (Config partitions channels _) = unlines $
  [ "digraph risk_config {"
  , concat [ printf "  %s [label=\"%s\\n%s, %d bytes\"];\n" name name (rate r) size | Partition name r size <- partitions ]
  , concatMap channel channels
  , "}"
  ]
  where
  rate :: Maybe Double -> String
  rate a = case a of
    Nothing -> "background"
    Just a  -> printf "%f Hz" a

  channel :: Channel -> String
  channel (Channel s sSize r rSize) = printf "  %s -> %s [label=\"%d -> %d\"];\n" s r sSize rSize

