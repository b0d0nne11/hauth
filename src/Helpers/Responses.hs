{-# LANGUAGE OverloadedStrings #-}

-- | This module contains helpful utility functions
module Helpers.Responses (
    -- * Response helpers
    writeJSON,
    writeKV,
    badRequest,
    notFound,
    serverError,
  ) where

import           Data.Aeson            (ToJSON, encode)
import qualified Data.ByteString.Char8 as C
import qualified Data.Map              as Map
import           Snap.Core             (finishWith, getResponse, logError,
                                        modifyResponse, setContentType,
                                        setResponseCode, writeLBS)
import           Text.Printf           (printf)

import           Application           (AppHandler)
import           Types                 ()

-- | Write a JSON response body
writeJSON :: ToJSON a => a -> AppHandler ()
writeJSON a = do
    modifyResponse $ setContentType "application/json"
    writeLBS $ encode a

-- | Write a JSON response body for a given key value pair
writeKV :: ToJSON a => String -> a -> AppHandler ()
writeKV k v = writeJSON $ Map.singleton k v

-- | Log an error and finish with a 400 Bad Request
badRequest :: String -> AppHandler a
badRequest e = do
    logError $ C.pack $ printf "bad request: %v" e
    writeKV "message" $ "bad request: " ++ e
    modifyResponse $ setResponseCode 400
    getResponse >>= finishWith

-- | Log an error and finish with a 404 Not Found
notFound :: String -> AppHandler a
notFound e = do
    logError $ C.pack $ printf "not found: %v" e
    writeKV "message" ("not found" :: C.ByteString)
    modifyResponse $ setResponseCode 404
    getResponse >>= finishWith

-- | Log an error and finish with a 500 Internal Server Error
serverError :: String -> AppHandler a
serverError e = do
    logError $ C.pack $ printf "server error: %v" e
    writeKV "message" ("internal server error" :: C.ByteString)
    modifyResponse $ setResponseCode 500
    getResponse >>= finishWith
