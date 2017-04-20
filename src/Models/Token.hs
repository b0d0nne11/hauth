{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains types pertaining to the 'Token' object
module Models.Token (
    AuthParams (..)
  ) where

import           Control.Applicative (empty)
import           Crypto.Scrypt       (Pass)
import           Data.Aeson          (FromJSON, ToJSON, object, parseJSON,
                                      toJSON, (.:), (.=))
import           Data.Aeson.Types    (Value (..))
import           Data.Text           (Text)

import           Schema              ()

-- | Authentication parameters
data AuthParams = AuthParams
    { _authParamsName :: Text
    , _authParamsPass :: Pass
    }

instance FromJSON AuthParams where
    parseJSON (Object v) = AuthParams <$> v .: "username"
                                      <*> v .: "password"
    parseJSON _ = empty

instance ToJSON AuthParams where
    toJSON (AuthParams name pass) =
        object [ "username" .= name
               , "password" .= pass
               ]
