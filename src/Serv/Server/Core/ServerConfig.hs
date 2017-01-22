{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Serv.Server.Core.ServerConfig
    ( ServerConfig(..)
    , setupServerConfig
    ) where

import           Data.Aeson
import           Data.Text          (Text)
import qualified Data.Yaml          as Y
import           GHC.Generics
import           System.Environment (lookupEnv)


data ServerConfig = ServerConfig
  { serverApiPort :: Int
  , serverSysPort :: Int
  , serverName    :: Text
  } deriving (Generic, Show)

defaultServerConfig = ServerConfig { serverApiPort = 8080, serverSysPort = 8090, serverName = "Bear" }

setupServerConfig :: IO ServerConfig
setupServerConfig = return defaultServerConfig
{-
loadConfig :: IO (Maybe RuntimeConig)
loadConfig = loadConfigFromFile "serv.yaml"

loadConfigFromFile :: String -> IO (Maybe RuntimeConig)
loadConfigFromFile = Y.decodeFile


getEnvVar :: Read a => String -> a -> IO a
getEnvVar name def = do
    maybeValue <- lookupEnv name
    return $ maybe def read maybeValue

  -}
