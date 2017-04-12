{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Serv.Server.Core.Runtime
    ( runCore
    ) where


import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader                 (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Aeson.Encode.Pretty             (encodePretty)
import qualified Data.ByteString.Lazy.Char8           as BL8
import           Data.Text.Encoding                   (encodeUtf8)
import           Network.Wai                          (Application)
import           Network.Wai.Handler.Warp             (run)
import qualified Network.Wai.Handler.Warp             as WP
import           Network.Wai.Metrics
import qualified Network.Wai.Middleware.Cors          as CS
import qualified Network.Wai.Middleware.Gzip          as GZ
import           Network.Wai.Middleware.RequestLogger
import qualified Network.Wai.Middleware.StripHeaders  as SH
import           Serv.Api
import           Serv.Api.Types
import           Serv.Server.Core.Config
import qualified Serv.Server.Core.Config              as Cfg
import           Serv.Server.Core.HealthApi
import           Serv.Server.Core.HealthHandler
import           Serv.Server.Core.InfoApi
import           Serv.Server.Core.ManagmentAuth
import           Serv.Server.Core.Metrics
import           Serv.Server.Core.MetricsApi
import           Serv.Server.Core.MetricsHandler
import           Serv.Server.Features.EntityHandler
import           Serv.Server.ServerEnv
import           Servant
import           Servant.Extentions.Server
import           System.Log.FastLogger


-- | System API: info, health, metrics
type SysApi = InfoApi :<|> HealthApi :<|> MetricsApi

coreServer :: ServerEnv -> Server SysApi
coreServer serverEnv = handleInfo serverEnv :<|> handleHealth serverEnv :<|> handleMetrics serverEnv

coreApp :: ServerEnv -> Application
coreApp serverEnv@ServerEnv{..} = serveWithContextEx (Proxy :: Proxy SysApi) (managementAuthContext (managementConf serverConfig)) (coreServer serverEnv)

runCore :: ServerEnv -> IO ()
runCore serverEnv@ServerEnv{..} =  do
  putStrLn ("[Sys] Listening on " ++ show port)
  WP.runSettings settings $ middleware $ coreApp serverEnv
  where
   middleware = GZ.gzip GZ.def . CS.simpleCors
   settings = WP.setServerName (encodeUtf8(serverName (serverConf serverConfig))) . WP.setPort port $ WP.defaultSettings
   port = Cfg.port (managementConf serverConfig)
