-- | RISK partitions specification.
module RISK.Spec
  ( Spec               (..)
  , Partition          (..)
  , Channel            (..)
  , ScheduleConstraint (..)
  , Name
  , graphviz
  , validateSpec
  ) where

import Text.Printf

-- | Partition names.
type Name = String

-- | Overall specification.
data Spec = Spec
  { partitions :: [Partition]              -- ^ The list of partitions.
  , channels   :: [Channel]                -- ^ The partition communication channels (sender, receiver, buffer size).
  , scheduling :: [ScheduleConstraint]     -- ^ Partition scheduling constraints.
  }

-- | Partition specification.
data Partition = Partition
  { pName   :: Name         -- ^ The partition name.
  , pRate   :: Maybe Double -- ^ Execution rate of partition in Hz.  If Nothing, thread runs in background.
  , pMemory :: Int          -- ^ The memory size allocated to the partition in log2 words.
  }

-- | Inter partition communication channel.
data Channel = Channel
  { cSender             :: Name       -- ^ Sending partition name.
  , cSenderBufferSize   :: Int        -- ^ Sending buffer size in log2 words.
  , cReceiver           :: Name       -- ^ Receiving partition name.
  , cReceiverBufferSize :: Int        -- ^ Receiving buffer size in log2 words.
  }

-- | Partition scheduling constraints.
data ScheduleConstraint
  -- | Execution priority between two partitions.  Overrides scheduling by channels if one exists.
  = Before Name Name

-- | Checks that a spec is valid.
validateSpec :: Spec -> Spec
validateSpec = id --XXX
  -- No unknown partition names.
  -- All buffer and memory sizes greater than 0.
  -- Partition memory size can hold all channel buffers.
  -- Q: Should loopback channels be allowed?
  -- Receive buffer sizes are power of 2.  (For quick circular buffer pointer calculations.)

-- | Generate a Graphviz diagram of a kernel specification.
graphviz :: Spec -> String
graphviz (Spec partitions channels _) = unlines $
  [ "digraph risk_config {"
  , concat [ printf "  %s [label=\"%s\\n%s, %d words\"];\n" name name (rate r) (2 ^ size :: Int) | Partition name r size <- partitions ]
  , concatMap channel channels
  , "}"
  ]
  where
  rate :: Maybe Double -> String
  rate a = case a of
    Nothing -> "background"
    Just a  -> printf "%f Hz" a

  channel :: Channel -> String
  channel (Channel s sSize r rSize) = printf "  %s -> %s [label=\"%d -> %d\"];\n" s r (2 ^ sSize :: Int) (2 ^ rSize :: Int)

