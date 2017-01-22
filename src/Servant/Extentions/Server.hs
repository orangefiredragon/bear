{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.Extentions.Server
    ( serveWithContextEx
    ) where

import           Network.Wai             (Application, Request, Response,
                                          ResponseReceived)
import           Servant
import           Servant.API
import           Servant.Server.Internal
import           Network.HTTP.Types.Header
import           Serv.Api.Types (failureReqBody)
import           Data.Aeson (encode)
import           Data.Text.Lazy.Encoding (decodeUtf8)


serveWithContextEx :: (HasServer api context)
    => Proxy api -> Context context -> Server api -> Application
serveWithContextEx p context server =
  toApplication' (runRouter (route p context (emptyDelayed (Route server))))

toApplication' :: RoutingApplication -> Application
toApplication' ra request respond = ra request routingRespond
 where
  routingRespond :: RouteResult Response -> IO ResponseReceived
  routingRespond (Fail err)      = respond $ responseServantErr (errBodyToJSON err)
  routingRespond (FailFatal err) = respond $ responseServantErr (errBodyToJSON err)
  routingRespond (Route v)       = respond v

errBodyToJSON :: ServantErr -> ServantErr
errBodyToJSON err@ServantErr{..} = case errHTTPCode of
  400 -> ServantErr
          { errHTTPCode = errHTTPCode
          , errReasonPhrase = errReasonPhrase
          , errBody = errToJSON
          , errHeaders = (hContentType, "application/json") : errHeaders}
  _   ->  err
  where
    errToJSON = encode $ failureReqBody $ decodeUtf8 errBody
