module Serv.Server
    ( bootstrap
    ) where

import           Control.Concurrent.Async
import           Serv.Server.Core.Runtime
import           Serv.Server.Features.Runtime
import           Serv.Server.ServerEnv

bootstrap :: IO ()
bootstrap =  do
       serverEnv <- setupServerEnv
       concurrently_ (runCore serverEnv) (runFeatures serverEnv)
