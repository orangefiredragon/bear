{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Serv.Server.Core.Config.ConfigData
    ( ConfigData (..)
    , validateConfig
    ) where

import           Data.Aeson                             (FromJSON (..),
                                                         ToJSON (..),
                                                         Value (..), object,
                                                         (.:), (.:?), (.=))

import qualified Data.Aeson                             as JS
import           Data.Monoid                            hiding ((<>))
import           Data.Semigroup                         (Semigroup (..))
import           Data.Text                              (Text)
import           GHC.Generics
import           Serv.Server.Core.Config.LogData
import           Serv.Server.Core.Config.ManagementData
import           Serv.Server.Core.Config.ServerData
import           Serv.Server.Core.Config.Types
import           Serv.Util.GenericMonoid
import           Serv.Util.Validate

data ConfigData = ConfigData (Last ServerData) (Last ManagementData) (Last LogData)
  deriving (Eq, Show, Generic)

instance Semigroup ConfigData where
  (<>) = gmappend

instance Monoid ConfigData where
  mempty  = gmempty
  mappend = (<>)

instance ToJSON ConfigData where
  toJSON (ConfigData server management lg) =
    object [ "server"      .= server
           , "management"  .= management
           , "log"         .= lg
           ]

instance FromJSON ConfigData where
  parseJSON =
    JS.withObject "ConfigData" $ \o ->
      ConfigData  <$> (Last <$> (o .:? "server"))
                  <*> (Last <$> (o .:? "management"))
                  <*> (Last <$> (o .:? "log"))


validateConfig :: ConfigData -> Either [String] Config
validateConfig (ConfigData s m l) =
   runValidate $
     Config <$> validateServer s
            <*> validateManagement m
            <*> validateLog defLogConfig l


defLogConfig = LogConfig [Console] None
