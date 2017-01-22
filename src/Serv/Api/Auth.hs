{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Serv.Api.Auth
    ( Principal(..)
    , basicAuthServerContext
    ) where

import           Data.Text             (Text)
import           Servant.API
import           Servant.API.BasicAuth (BasicAuthData (BasicAuthData))
import           Servant.Server        (BasicAuthCheck (BasicAuthCheck), BasicAuthResult (Authorized, Unauthorized),
                                        Context ((:.), EmptyContext), Handler,
                                        Server, err401, err403, errBody,
                                        serveWithContext)

data Principal = Principal
  { principalName :: Text
  } deriving (Eq, Show)

-- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
authCheck :: BasicAuthCheck Principal
authCheck =
  let check (BasicAuthData username password) =
        if username == "servant" && password == "server"
        then return (Authorized (Principal "servant"))
        else return Unauthorized
  in BasicAuthCheck check

basicAuthServerContext :: Context (BasicAuthCheck Principal ': '[])
basicAuthServerContext = authCheck :. EmptyContext
