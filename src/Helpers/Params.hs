{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | This module contains helpful utility functions
module Helpers.Params (
    -- * Param helpers
    requireParam,
    getTextParam,
    getIntParam,
    getEmailParam,
    getEncryptedParam,
    getKeyParam,
    getPageParam,
    getRequestBody
  ) where

import           Crypto.Scrypt         (EncryptedPass (..), Pass (..))
import           Data.Aeson            (FromJSON, decode)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import           Data.Int              (Int64)
import           Data.Maybe            (fromMaybe)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Database.Persist      (Key, ToBackendKey)
import           Database.Persist.Sql  (SqlBackend, toSqlKey)
import           Snap.Core             (getParam, readRequestBody)
import qualified Text.Email.Validate   as Email
import           Text.Printf           (printf)
import           Text.Read             (readEither)

import           Application           (AppHandler)
import           Helpers.Crypto        (encrypt)
import           Helpers.Responses     (badRequest)
import           Models.Page           (Page (..))
import           Types                 ()

-- | Extract the element out of a request parameter or finish with a 400 Bad Request
requireParam :: (BS.ByteString -> AppHandler (Maybe a)) -> BS.ByteString -> AppHandler a
requireParam getter key = do
    mParam <- getter key
    case mParam of
        Nothing -> badRequest $ printf "missing required parameter: %v" $ C.unpack key
        Just param -> return param

-- | Get a text value from a request parameter
getTextParam :: BS.ByteString -> AppHandler (Maybe T.Text)
getTextParam key = do
    mParam <- getParam key
    case mParam of
        Nothing -> return Nothing
        Just param -> return $ Just $ T.decodeUtf8 param

-- | Get an integer value from a request parameter
getIntParam :: BS.ByteString -> AppHandler (Maybe Int64)
getIntParam key = do
    mParam <- getParam key
    case mParam of
        Nothing -> return Nothing
        Just param ->
            case readEither $ C.unpack param of
                Left _ -> badRequest $ printf "invalid integer parameter: %v=%v" (C.unpack key) (C.unpack param)
                Right int -> return $ Just int

-- | Get an email address value from a request parameter
getEmailParam :: BS.ByteString -> AppHandler (Maybe Email.EmailAddress)
getEmailParam key = do
    mParam <- getParam key
    case mParam of
        Nothing -> return Nothing
        Just param ->
            case Email.validate param of
                Left _ -> badRequest $ printf "invalid email parameter: %v=%v" (C.unpack key) (C.unpack param)
                Right email -> return $ Just email

-- | Get an encrypted value from a request parameter
getEncryptedParam :: BS.ByteString -> AppHandler (Maybe EncryptedPass)
getEncryptedParam key = do
    mParam <- getParam key
    case mParam of
        Nothing -> return Nothing
        Just param -> do
            encryptedParam <- encrypt $ Pass param
            return $ Just encryptedParam

-- | Get a backend key value from a request parameter
getKeyParam :: ToBackendKey SqlBackend record => BS.ByteString -> AppHandler (Maybe (Key record))
getKeyParam key = do
    mParam <- getParam key
    case mParam of
        Nothing -> return Nothing
        Just param ->
            case readEither $ C.unpack param of
                Left _ -> badRequest $ printf "invalid key parameter: %v=%v" (C.unpack key) (C.unpack param)
                Right int -> return $ Just $ toSqlKey int

-- | Get pagination parameters from the request parameters
getPageParam :: ToBackendKey SqlBackend record => AppHandler (Page record)
getPageParam = Page <$> (getParam "order" >>= toOrder)
                    <*> (getIntParam "limit" >>= toLimit)
                    <*> getKeyParam "before"
                    <*> getKeyParam "after"
    where
      toLimit = return . max 0 . min 1000 . fromMaybe 100
      toOrder mParam = case mParam of
          Nothing -> return "asc"
          Just "asc" -> return "asc"
          Just "desc" -> return "desc"
          Just _ -> badRequest "invalid page order"

-- | Get a set of values from the request body
getRequestBody :: FromJSON a => AppHandler a
getRequestBody = do
    body <- readRequestBody maxLength
    case decode body of
        Nothing -> badRequest "invalid JSON body"
        Just json -> return json
  where
    maxLength = 1024 * 64 -- 64 kB
