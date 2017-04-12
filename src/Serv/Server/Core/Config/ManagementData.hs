{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Serv.Server.Core.Config.ManagementData
    ( ManagementData (..)
    , validateManagement
    ) where

import           Data.Aeson                    (FromJSON (..), ToJSON (..),
                                                Value (..), object, (.:), (.:?),
                                                (.=))

import qualified Data.Aeson                    as JS
import           Data.Monoid                   hiding ((<>))
import           Data.Semigroup                (Semigroup (..))
import           Data.Text                     (Text)
import           GHC.Generics
import           Serv.Server.Core.Config.Types
import           Serv.Util.GenericMonoid
import           Serv.Util.Validate


data ManagementData = ManagementData (Last Port) (Last UserName) (Last UserPassword)
  deriving (Eq, Show, Generic)

instance Semigroup ManagementData where
  (<>) = gmappend

instance Monoid ManagementData where
  mempty  = gmempty
  mappend = (<>)

instance ToJSON ManagementData where
  toJSON (ManagementData port userName userPassword) =
    object [ "port"           .= port
            , "userName"     .= userName
            , "userPassword" .= userPassword
           ]

instance FromJSON ManagementData where
  parseJSON =
    JS.withObject "ManagementData" $ \o ->
      ManagementData  <$> (Last <$> (o .:? "port"))
                            <*> (Last <$> (o .:? "userName"))
                            <*> (Last <$> (o .:? "userPassword"))

-- TODO: combine all invalids
validateManagement :: Last ManagementData -> Validate ManagementConfig
validateManagement x = do
  (ManagementData prt usr pswd) <- requiredField "Management configuration is required" x
  port <- requiredField "Management port is required" prt
  username <- requiredField "Management userName is required" usr
  password <- requiredField "Management userPassword is required" pswd
  return (ManagementConfig port username password)
