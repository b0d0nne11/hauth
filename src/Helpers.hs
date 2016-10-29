{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}

-- | This module contains helpful utility functions
module Helpers (
    -- * Responses
    writeJSON,
    writeKV,
    badRequest,
    notFound,
    serverError,
    -- * Parameter parsing
    requireParam,
    getTextParam,
    getEmailParam,
    getEncryptedParam,
    getKeyParam,
    getKeyParams,
    getInt64Param,
    getRequestBody,
    -- * Pagination
    Page(..),
    getPageParam,
    -- * Crypto functions
    encrypt,
    verify,
    -- * Crypto re-exports
    Pass(..),
    EncryptedPass(..),
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Crypto.Scrypt          (EncryptedPass (..), Pass (..),
                                         defaultParams, encryptPassIO,
                                         verifyPass)
import           Data.Aeson             (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as C
import           Data.Either            (partitionEithers)
import           Data.Int               (Int64)
import qualified Data.Map               as Map
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import           Database.Persist       (Key, ToBackendKey)
import           Database.Persist.Sql   (SqlBackend, toSqlKey)
import           Snap.Core              (finishWith, getParam, getResponse,
                                         logError, modifyResponse,
                                         readRequestBody, setContentType,
                                         setResponseCode, writeLBS)
import qualified Text.Email.Validate    as Email
import           Text.Printf            (printf)
import           Text.Read              (readEither)

import           Application            (AppHandler)
import           Types                  ()

-- | Write a JSON response body
writeJSON :: ToJSON a => a -> AppHandler ()
writeJSON a = do
    writeLBS $ encode a
    modifyResponse $ setContentType "application/json"

-- | Write a JSON response body for a given key value pair
writeKV :: ToJSON a => String -> a -> AppHandler ()
writeKV k v = writeJSON $ Map.singleton k v

-- | Log an error and finish with a 400 Bad Request
badRequest :: String -> AppHandler a
badRequest e = do
    logError $ C.pack $ printf "bad request: %v" e
    writeKV "message" e
    modifyResponse $ setResponseCode 400
    getResponse >>= finishWith

-- | Log an error and finish with a 404 Not Found
notFound :: String -> AppHandler a
notFound e = do
    logError $ C.pack $ printf "not found: %v" e
    writeKV "message" ("Not found" :: C.ByteString)
    modifyResponse $ setResponseCode 404
    getResponse >>= finishWith

-- | Log an error and finish with a 500 Internal Server Error
serverError :: String -> AppHandler a
serverError e = do
    logError $ C.pack $ printf "server error: %v" e
    writeKV "message" ("Internal server error" :: C.ByteString)
    modifyResponse $ setResponseCode 500
    getResponse >>= finishWith

-- | Extract the element out of a request parameter or finish with a 400 Bad Request
requireParam :: (BS.ByteString -> AppHandler (Maybe a)) -> BS.ByteString -> AppHandler a
requireParam getter key = do
    param <- getter key
    case param of
        Nothing -> badRequest errMessage
        Just a -> return a
  where
    errMessage = "Bad request: missing " ++ C.unpack key

-- | Get an email address value from a request parameter
getEmailParam :: BS.ByteString -> AppHandler (Maybe Email.EmailAddress)
getEmailParam key = do
    mparam <- getParam key
    case mparam of
        Nothing -> return Nothing
        Just param ->
            case Email.validate param of
                Left _ -> badRequest errMessage
                Right email -> return $ Just email
  where
    errMessage = "Bad request: invalid " ++ C.unpack key

-- | Get a text value from a request parameter
getTextParam :: BS.ByteString -> AppHandler (Maybe T.Text)
getTextParam key = do
    mparam <- getParam key
    case mparam of
        Nothing -> return Nothing
        Just param -> return $ Just $ T.decodeUtf8 param

-- | Get an integer value from a request parameter
getInt64Param :: BS.ByteString -> AppHandler (Maybe Int64)
getInt64Param key = do
    mparam <- getParam key
    case mparam of
        Nothing -> return Nothing
        Just param ->
            case readEither $ C.unpack param of
                Left _ -> badRequest errMessage
                Right int -> return $ Just int
  where
    errMessage = "bad request: invalid " ++ C.unpack key

-- | Get an encrypted value from a request parameter
getEncryptedParam :: BS.ByteString -> AppHandler (Maybe EncryptedPass)
getEncryptedParam key = do
    mparam <- getParam key
    case mparam of
        Nothing -> return Nothing
        Just param -> do
            encryptedParam <- encrypt $ Pass param
            return $ Just encryptedParam

-- | Get a backend key value from a request parameter
getKeyParam :: ToBackendKey SqlBackend record => BS.ByteString -> AppHandler (Maybe (Key record))
getKeyParam key = do
    mparam <- getParam key
    case mparam of
        Nothing -> return Nothing
        Just param ->
            case readEither $ C.unpack param of
                Left _ -> badRequest errMessage
                Right int -> return $ Just $ toSqlKey int
  where
    errMessage = "Bad request: invalid " ++ C.unpack key

-- | Get a list of backend key values from a request parameter
getKeyParams :: ToBackendKey SqlBackend record => BS.ByteString -> AppHandler [Key record]
getKeyParams key = do
    mparam <- getParam key
    case C.split ' ' <$> mparam of
        Nothing -> return []
        Just params ->
            case partitionEithers $ map (readEither . C.unpack) params of
                ([], ints) -> return $ map toSqlKey ints
                _ -> badRequest errMessage
  where
    errMessage = "Bad request: invalid " ++ C.unpack key

-- | Get a set of values from the request body
getRequestBody :: FromJSON a => AppHandler a
getRequestBody = do
    body <- readRequestBody maxLength
    case decode body of
        Nothing -> badRequest errMessage
        Just json -> return json
  where
    errMessage = "Bad request: invalid JSON body"
    maxLength = 1024 * 64 -- 64 kB

-- | Generate an encrypted field from cleartext
encrypt :: Pass -> AppHandler EncryptedPass
encrypt pass = liftIO $ encryptPassIO defaultParams pass

-- | Verify a cleartext password matches an encrypted field
verify :: Pass -> EncryptedPass -> (Bool, Maybe EncryptedPass)
verify = verifyPass defaultParams

-- | Pagination parameters
data Page record = ToBackendKey SqlBackend record =>
    Page { pageOrder  :: BS.ByteString
         , pageLimit  :: Int64
         , pageBefore :: Maybe (Key record)
         , pageAfter  :: Maybe (Key record)
         }

-- | Get pagination parameters from the request parameters
getPageParam :: ToBackendKey SqlBackend record => AppHandler (Page record)
getPageParam = Page <$> (getParam "order" >>= toOrder)
                    <*> (getInt64Param "limit" >>= toLimit)
                    <*> getKeyParam "before"
                    <*> getKeyParam "after"
    where
      toLimit = return . max 0 . min 100 . fromMaybe 100
      toOrder = \mparam -> case mparam of
          Nothing -> return "asc"
          Just "asc" -> return "asc"
          Just "desc" -> return "desc"
          Just _ -> badRequest "bad request: invalid order"
