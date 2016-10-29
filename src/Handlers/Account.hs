{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains API handlers pertaining to the 'Account' object
module Handlers.Account (
    -- * API handlers
    listAccountsApi,
    showAccountApi,
    createAccountApi,
    updateAccountApi,
    deleteAccountApi,
    -- * Utility functions
    accountUrl
  ) where

import           Data.Aeson            (ToJSON, object, toJSON, (.=))
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import           Database.Persist      (Entity (..))
import           Snap.Core             (modifyResponse, redirect,
                                        setResponseCode)
import           Text.Printf           (printf)

import           Application           (AppHandler)
import           Helpers               (getKeyParam, getPageParam,
                                        getRequestBody, getTextParam,
                                        requireParam, writeJSON)
import           Models.Account        (CreateParams, UpdateParams,
                                        deleteAccount, getDomainAccount,
                                        insertAccount, newAccount,
                                        selectAccounts, updateAccount,
                                        updateAccountUsers)
import           Schema

instance ToJSON (Entity Account) where
    toJSON  (Entity key (Account domainId name)) =
        object [ "id"         .= key
               , "domain_id"  .= domainId
               , "name"       .= name
               ]

-- | Get an account link
accountUrl :: Entity Account -- ^ Account
           -> BS.ByteString  -- ^ Returns a link to the account
accountUrl (Entity accountId account) = C.pack $ printf "/domains/%v/accounts/%v" (accountDomainId account) accountId

-- | The API handler for GET /domains/:domain_id/accounts
listAccountsApi :: AppHandler ()
listAccountsApi = do
    domainId <- requireParam getKeyParam "domain_id"
    filterName <- getTextParam "name"
    filterUserId <- getKeyParam "user_id"
    page <- getPageParam
    accounts <- selectAccounts domainId filterName filterUserId page
    writeJSON (accounts :: [Entity Account])

-- | The API handler for GET /domains/:domain_id/accounts/:account_id
showAccountApi :: AppHandler ()
showAccountApi = do
    domainId <- requireParam getKeyParam "domain_id"
    accountId <- requireParam getKeyParam "account_id"
    account <- getDomainAccount domainId accountId
    writeJSON (account :: Entity Account)

-- | The API handler for POST/PUT /domains/:domain_id/accounts
createAccountApi :: AppHandler ()
createAccountApi = do
    account <- newAccount <$> requireParam getKeyParam "domain_id"
                          <*> (getRequestBody :: AppHandler CreateParams)
    accountId <- insertAccount account
    redirect $ accountUrl $ Entity accountId account

-- | The API handler for POST/PUT /domains/:domain_id/accounts/:account_id
updateAccountApi :: AppHandler ()
updateAccountApi = do
    domainId <- requireParam getKeyParam "domain_id"
    accountId <- requireParam getKeyParam "account_id"
    params <- (getRequestBody :: AppHandler UpdateParams)
    _ <- getDomainAccount domainId accountId
    updateAccount accountId params
    updateAccountUsers accountId params
    modifyResponse $ setResponseCode 204

-- | The API handler for DELETE /domains/:domain_id/accounts/:account_id
deleteAccountApi :: AppHandler ()
deleteAccountApi = do
    domainId <- requireParam getKeyParam "domain_id"
    accountId <- requireParam getKeyParam "account_id"
    _ <- getDomainAccount domainId accountId
    deleteAccount accountId
    modifyResponse $ setResponseCode 204
