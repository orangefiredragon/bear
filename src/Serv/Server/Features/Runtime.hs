{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Serv.Server.Features.Runtime
    ( runFeatures
    ) where

import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader                 (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Aeson.Encode.Pretty             (encodePretty)
import qualified Data.ByteString.Lazy.Char8           as BL8
import qualified Data.HashMap.Strict                  as H
import           Data.Swagger
import           Data.Text.Encoding                   (encodeUtf8)
import           Network.Wai                          (Application)
import qualified Network.Wai.Handler.Warp             as WP
import qualified Network.Wai.Middleware.Cors          as CS
import qualified Network.Wai.Middleware.Gzip          as GZ
import qualified Network.Wai.Middleware.RequestLogger as RL
import qualified Network.Wai.Middleware.StripHeaders  as SH
import           Serv.Api
import           Serv.Api.Auth
import           Serv.Server.Core.Logger              (LogEnv (..))
import           Serv.Server.Core.Metrics
import           Serv.Server.Core.ServerConfig
import           Serv.Server.Features.EntityHandler
import           Serv.Server.ServerEnv
import           Servant
import           Servant.Ekg                          (monitorEndpoints)
import           Servant.Extentions.Server
import           Servant.Swagger

-- | Features APIs
type FeaturesAPI = EntityV1Api

-- | API for serving API documentation
type DocAPI = "v1" :> "doc" :> Get '[JSON] Swagger

-- | Features API Swagger documentation
type API = DocAPI :<|> FeaturesAPI


featuresAPI :: Proxy FeaturesAPI
featuresAPI = Proxy

-- | Swagger spec for Entity V1 API.
docAPI :: Swagger
docAPI = toSwagger featuresAPI
  & info.title   .~ "Service API"
  & info.version .~ "1.0"
  & info.description ?~ "This is Service API that does many things"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

featuresServer :: ServerEnv -> Server API
featuresServer serverEnv = return docAPI :<|> handlePostEntity serverEnv  :<|> handleGetEntity serverEnv  :<|> handlePutEntity serverEnv  :<|> handleDeleteEntity serverEnv

featuresApp :: ServerEnv -> Application
featuresApp serverEnv = serveWithContextEx (Proxy :: Proxy API) basicAuthServerContext (featuresServer serverEnv)

runFeatures :: ServerEnv -> IO ()
runFeatures  serverEnv@ServerEnv{..} =  do
  ms <- newMVar H.empty
  let featuresMetrics = monitorEndpoints featuresAPI (metricsStore serverMetrics) ms
  putStrLn ("[API] Listening on " ++ show port)
  WP.runSettings settings $ middleware . featuresMetrics $ featuresApp serverEnv
  where
   middleware = maybe (GZ.gzip GZ.def . CS.simpleCors) (. GZ.gzip GZ.def . CS.simpleCors) (logMiddleware logEnv)
   settings = WP.setServerName (encodeUtf8(serverName serverConfig)) . WP.setPort port $ WP.defaultSettings
   port = serverApiPort serverConfig
