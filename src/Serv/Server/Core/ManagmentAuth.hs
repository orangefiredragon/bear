{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Serv.Server.Core.ManagmentAuth
    ( Principal(..)
    , managementAuthContext
    ) where

import           Data.Text               (Text)
import qualified Data.Text.Encoding      as TE
import           Serv.Server.Core.Config (ManagementConfig (..))
import           Servant.API
import           Servant.API.BasicAuth   (BasicAuthData (BasicAuthData))
import           Servant.Server          (BasicAuthCheck (BasicAuthCheck), BasicAuthResult (Authorized, Unauthorized),
                                          Context ((:.), EmptyContext), Handler,
                                          Server, err401, err403, errBody,
                                          serveWithContext)


newtype Principal = Principal
  { principalName :: Text
  } deriving (Eq, Show)

-- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
managementAuthCheck :: ManagementConfig -> BasicAuthCheck Principal
managementAuthCheck conf =
  let check (BasicAuthData username password) =
        if TE.decodeUtf8 username == userName conf && TE.decodeUtf8 password == userPassword conf
          then return (Authorized (Principal (TE.decodeUtf8 username)))
          else return Unauthorized
  in BasicAuthCheck check

managementAuthContext :: ManagementConfig -> Context (BasicAuthCheck Principal ': '[])
managementAuthContext conf = managementAuthCheck conf :. EmptyContext
