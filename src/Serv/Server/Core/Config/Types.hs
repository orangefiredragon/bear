{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Serv.Server.Core.Config.Types
    ( Config(..)
    , LogAppender(..)
    , LogConfig(..)
    , ManagementConfig(..)
    , Port
    , ServerConfig(..)
    , ServerName
    , UserName
    , UserPassword
    ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics

type Port = Int
type Secret = Text
type ServerName = Text
type UserPassword = Text
type UserName = Text

data ManagementConfig = ManagementConfig
  { port         :: Port
  , userName     :: UserName
  , userPassword :: UserPassword
  } deriving (Eq, Show)

data LogAppender = None | Console | File
   deriving (Eq, Generic, Show, ToJSON, FromJSON)

data LogConfig = LogConfig
  { appenders :: [LogAppender]
  , request   :: LogAppender
  } deriving (Generic, Show)


data ServerConfig = ServerConfig
  { serverPort :: Port
  , serverName :: ServerName
  } deriving (Generic, Show)

data Config = Config
  { serverConf     :: ServerConfig
  , managementConf :: ManagementConfig
  , logConf        :: LogConfig
  } deriving (Generic, Show)
