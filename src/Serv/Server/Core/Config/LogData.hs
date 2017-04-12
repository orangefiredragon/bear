{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Serv.Server.Core.Config.LogData
    ( LogData(..)
    , validateLog
    ) where

import           Control.Exception
import           Data.Aeson                    (FromJSON (..), ToJSON (..),
                                                Value (..), object, (.:), (.:?),
                                                (.=))
import qualified Data.Aeson                    as JS
import qualified Data.Aeson.Types              as JS
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as LBS
import           Data.Monoid                   hiding ((<>))
import           Data.Semigroup                (Semigroup (..))
import           Data.Text                     (Text)
import qualified Data.Yaml                     as Y
import qualified Data.Yaml                     as Yaml
import           GHC.Generics
import           Serv.Server.Core.Config.Types
import           Serv.Util.GenericMonoid
import           Serv.Util.Validate

-- TODO rethink
validateLog :: LogConfig -> Last LogData -> Validate LogConfig
validateLog def (Last Nothing)                    = return  def
validateLog _   (Last (Just (LogData as (Last Nothing)))) = return (LogConfig as None)
validateLog _   (Last (Just (LogData as (Last (Just r))))) = return (LogConfig as r)

-- Raw Config file handling

data LogData = LogData [LogAppender] (Last LogAppender)
  deriving (Eq, Show, Generic)

instance Semigroup LogData where
  (<>) = gmappend

instance Monoid LogData where
  mempty  = gmempty
  mappend = (<>)

instance ToJSON LogData where
  toJSON (LogData appenders request) =
    object [ "appenders"    .= appenders
           , "request"      .= request
           ]

instance FromJSON LogData where
  parseJSON =
    JS.withObject "LogData" $ \o ->
      LogData <$> (o .: "appenders")
              <*> (Last <$> (o .:? "request"))
