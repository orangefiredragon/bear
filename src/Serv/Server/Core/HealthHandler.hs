module Serv.Server.Core.HealthHandler
    ( handleHealth
    ) where

import           Serv.Server.Core.HealthTypes
import           Serv.Server.ServerEnv
import           Servant

handleHealth :: ServerEnv -> Handler HealthResponse
handleHealth serverEnv = return healthUp
