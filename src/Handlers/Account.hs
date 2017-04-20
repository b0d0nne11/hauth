{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains API handlers pertaining to the 'Account' object
module Handlers.Account (
    -- * API handlers
    listAccountsHandler,
    showAccountHandler,
    createAccountHandler,
    updateAccountHandler,
    deleteAccountHandler,
    -- * Utility functions
    accountUrl
  ) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import           Database.Persist      (Key)
import           Snap.Core             (modifyResponse, redirect,
                                        setResponseCode)
import           Text.Printf           (printf)

import           Application           (AppHandler)
import           Helpers.Params        (getKeyParam, getPageParam,
                                        getRequestBody, getTextParam,
                                        requireParam)
import           Helpers.Responses     (writeJSON)
import           Models.Account        (deleteAccount, getAccount,
                                        insertAccount, newAccount,
                                        selectAccounts, updateAccount)
import           Schema

-- | Get an account link
accountUrl :: Key Account   -- ^ Account ID
           -> BS.ByteString -- ^ Returns a link to the account
accountUrl accountId = C.pack $ printf "/accounts/%v" accountId

-- | The API handler for GET /accounts
listAccountsHandler :: AppHandler ()
listAccountsHandler = do
    filterName <- getTextParam "name"
    page <- getPageParam
    accounts <- selectAccounts filterName page
    writeJSON accounts

-- | The API handler for GET /accounts/:account_id
showAccountHandler :: AppHandler ()
showAccountHandler = do
    accountId <- requireParam getKeyParam "account_id"
    account <- getAccount accountId
    writeJSON account

-- | The API handler for POST/PUT /accounts
createAccountHandler :: AppHandler ()
createAccountHandler = do
    params <- getRequestBody
    accountId <- newAccount params >>= insertAccount
    redirect $ accountUrl accountId

-- | The API handler for POST/PUT /accounts/:account_id
updateAccountHandler :: AppHandler ()
updateAccountHandler = do
    accountId <- requireParam getKeyParam "account_id"
    params <- getRequestBody
    _ <- getAccount accountId
    updateAccount accountId params
    modifyResponse $ setResponseCode 204

-- | The API handler for DELETE /accounts/:account_id
deleteAccountHandler :: AppHandler ()
deleteAccountHandler = do
    accountId <- requireParam getKeyParam "account_id"
    _ <- getAccount accountId
    deleteAccount accountId
    modifyResponse $ setResponseCode 204
