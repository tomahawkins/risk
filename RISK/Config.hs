-- | RISK configuration.
module RISK.Config
  ( Config             (..)
  , Partition          (..)
  , ChannelType        (..)
  , ScheduleConstraint (..)
  , Name
  , graphviz
  ) where

import Text.Printf

-- | Partition names.
type Name = String

-- | Overal kernel configuration.
data Config = Config
  { partitions :: [Partition]                  -- ^ The list of partitions.
  , channels   :: [(Name, Name, ChannelType)]  -- ^ The partition communication channels.
  , scheduling :: [ScheduleConstraint]         -- ^ Partition scheduling constraints.
  }

-- | Partition configuration.
data Partition = Partition
  { pName   :: Name     -- ^ The partition name.
  , pRate   :: Integer  -- ^ Execution rate of partition in Hz.
  , pMemory :: Integer  -- ^ The memory size allocated to the partition in bytes.
  }

-- | Inter partition communication channel types.
data ChannelType
  -- | Message passing channel with given buffer size.  If sender overfills buffer, data is lost (no back channel).
  = MessagePassing Integer
  -- | Message passing channel with given buffer size.  Back channel allowed, so sender can check buffer state and can block if buffer is full.
  | MessagePassingAllowBackChannel Integer

-- | Partition scheduling constraints.
data ScheduleConstraint
  -- | Execution priority between two partitions.  Overrides scheduling by channels if one exists.
  = Before Name Name

-- | Generate a Graphviz diagram of a kernel configuration.
graphviz :: Config -> String
graphviz (Config partitions channels _) = unlines $
  [ "digraph risk_config {"
  , concat [ printf "  %s [label=\"%s\\n%d Hz, %d bytes\"];\n" name name rate size | Partition name rate size <- partitions ]
  , concatMap channel channels
  , "}"
  ]
  where
  channel (from, to, typ) = case typ of
    MessagePassing                 size -> printf "  %s -> %s [label=\"%d\"];\n" from to size
    MessagePassingAllowBackChannel size -> printf "  %s -> %s [label=\"%d (allow backchannel)\"];\n" from to size


