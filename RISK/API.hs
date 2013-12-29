-- | RISK API.
module RISK.API
  (
  ) where

{-
yield  -- Yield thread to the scheduler.
time   -- Get the current execution time.  Q: Real time or step time?  Will real time create timing channel?
send             -- Send data on a given channel.  Non blocking.
sendBlocking     -- Send message, block if channel buffer full.  Only for AllowBackChannel.
sendSpaceAvail   -- Space available in given channel buffer.  Only for AllowBackChannel.
recvHasData      -- Check if channel has data to receive.
recv             -- Receive data on a given channel.
-}

