{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Serv.Server.Features.Runtime
    ( runFeatures
    ) where


import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader                 (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Aeson.Encode.Pretty             (encodePretty)
import qualified Data.ByteString.Lazy.Char8           as BL8
import           Data.Swagger
import           Network.Wai                          (Application)
import qualified Network.Wai.Handler.Warp             as WP
import           Network.Wai.Metrics
import qualified Network.Wai.Middleware.Cors          as CS
import qualified Network.Wai.Middleware.Gzip          as GZ
import qualified Network.Wai.Middleware.RequestLogger as RL
import qualified Network.Wai.Middleware.StripHeaders  as SH
import           Serv.Api
import           Serv.Api.Auth
import           Serv.Server.Core.Metrics
import           Serv.Server.Core.ServerConfig
import           Serv.Server.Features.EntityHandler
import           Serv.Server.ServerEnv
import           Servant
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
runFeatures  serverEnv =  do
  putStrLn ("[API] Listening on " ++ show port)
  WP.runSettings settings $ middleware $ featuresApp serverEnv
  where
   -- RL.mkRequestLogger RL.def
   middleware = GZ.gzip GZ.def . CS.simpleCors . metricsMiddleware . RL.logStdoutDev
   metricsMiddleware = metrics (waiMetrics $ serverMetrics serverEnv)
   settings = WP.setServerName "Bear" . WP.setPort port $ WP.defaultSettings
   port = serverApiPort (serverConfig serverEnv)
