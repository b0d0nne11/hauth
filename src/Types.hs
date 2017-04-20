{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Types where

import           Crypto.Scrypt        (EncryptedPass (..), Pass (..))
import           Data.Aeson           (FromJSON, ToJSON, Value (..), parseJSON,
                                       toJSON)
import           Data.Aeson.Types     (typeMismatch)
import           Data.ByteString      as BS
import           Data.Text            as T
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)
import           Database.Persist.Sql
import           Text.Email.Parser    (EmailAddress, toByteString)
import           Text.Email.Validate  (validate)
import           Text.Printf          (PrintfArg, errorBadFormat, fmtChar,
                                       formatArg, formatInt, formatString)

import           Snap.Snaplet.JWT     as JWT

-- Printf arguments

instance PrintfArg JWT.Error where
    formatArg x fmt
        | fmtChar fmt == 's' = formatString (show x) fmt
        | fmtChar fmt == 'v' = formatString (show x) fmt
        | otherwise          = errorBadFormat $ fmtChar fmt

instance ToBackendKey SqlBackend record => PrintfArg (Key record) where
    formatArg x fmt
        | fmtChar fmt == 'd' = formatInt (fromSqlKey x) fmt
        | fmtChar fmt == 'u' = formatInt (fromSqlKey x) fmt
        | fmtChar fmt == 'v' = formatInt (fromSqlKey x) fmt
        | otherwise          = errorBadFormat $ fmtChar fmt

instance ToBackendKey SqlBackend record => PrintfArg (Unique record) where
    formatArg x fmt
        | fmtChar fmt == 's' = formatString (show $ persistUniqueToValues x) fmt
        | fmtChar fmt == 'v' = formatString (show $ persistUniqueToValues x) fmt
        | otherwise          = errorBadFormat $ fmtChar fmt

-- JSON

instance FromJSON BS.ByteString where
    parseJSON (String v) = pure $ encodeUtf8 v
    parseJSON unknown = typeMismatch "ByteString" unknown

instance ToJSON BS.ByteString where
    toJSON = String . decodeUtf8

instance FromJSON Pass where
    parseJSON (String v) = pure $ Pass $ encodeUtf8 v
    parseJSON unknown = typeMismatch "Password" unknown

instance ToJSON Pass where
    toJSON = String . decodeUtf8 . getPass

instance FromJSON EmailAddress where
    parseJSON (String v) = either fail pure $ validate $ encodeUtf8 v
    parseJSON unknown = typeMismatch "EmailAddress" unknown

instance ToJSON EmailAddress where
    toJSON = String . decodeUtf8 . toByteString

-- SQL fields

instance PersistFieldSql EncryptedPass where
    sqlType _ = SqlBlob

instance PersistField EncryptedPass where
    toPersistValue (EncryptedPass pass) = PersistByteString pass

    fromPersistValue (PersistByteString pass) = Right $ EncryptedPass pass
    fromPersistValue _ = Left "EncryptedPass must be converted from PersistByteString"

instance PersistFieldSql EmailAddress where
    sqlType _ = SqlBlob

instance PersistField EmailAddress where
    toPersistValue email = PersistByteString $ toByteString email

    fromPersistValue (PersistByteString email) = either (Left . T.pack) Right $ validate email
    fromPersistValue _ = Left "EmailAddress must be converted from PersistByteString"
