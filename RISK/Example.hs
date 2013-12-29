-- | A RISK configuration example.
module RISK.Example
  ( exampleConfig
  ) where

import RISK.Config

-- | A RISK configuration example.
exampleConfig :: Config
exampleConfig = Config
  { partitions =
    [ Partition { pName = "sensor_1",       pRate = 1000,  pMemory = 1024 }
    , Partition { pName = "sensor_2",       pRate = 1000,  pMemory = 1024 }
    , Partition { pName = "sensor_3",       pRate = 1000,  pMemory = 1024 }
    , Partition { pName = "sensor_voting",  pRate = 1000,  pMemory = 1024 }
    , Partition { pName = "control_law_1",  pRate =  500,  pMemory = 1024 }
    , Partition { pName = "control_law_2",  pRate = 1000,  pMemory = 1024 }
    , Partition { pName = "actuator_1",     pRate =  500,  pMemory = 1024 }
    , Partition { pName = "actuator_2",     pRate =  500,  pMemory = 1024 }
    , Partition { pName = "actuator_3",     pRate = 1000,  pMemory = 1024 }
    ]
  , channels =
    [ ("sensor_1",       "sensor_voting",     MessagePassingAllowBackChannel 64)
    , ("sensor_2",       "sensor_voting",     MessagePassingAllowBackChannel 64)
    , ("sensor_3",       "sensor_voting",     MessagePassingAllowBackChannel 64)
    , ("sensor_voting",  "control_law_1",     MessagePassing 64)
    , ("sensor_voting",  "control_law_2",     MessagePassing 64)
    , ("control_law_1",  "actuator_1",        MessagePassing 64)
    , ("control_law_1",  "actuator_2",        MessagePassing 64)
    , ("control_law_2",  "actuator_3",        MessagePassing 64)
    ]
  , scheduling = []
  }

