{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains API handlers pertaining to the 'User' object
module Handlers.User (
    -- * API handlers
    listUsersHandler,
    showUserHandler,
    createUserHandler,
    updateUserHandler,
    deleteUserHandler,
    -- * Utility functions
    userUrl
  ) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import           Database.Persist      (Key)
import           Snap.Core             (modifyResponse, redirect,
                                        setResponseCode)
import           Text.Printf           (printf)

import           Application           (AppHandler)
import           Helpers.Params        (getEmailParam, getKeyParam,
                                        getPageParam, getRequestBody,
                                        getTextParam, requireParam)
import           Helpers.Responses     (writeJSON)
import           Models.User           (deleteUser, getUser, insertUser,
                                        newUser, selectUsers, updateUser)
import           Schema

-- | Get a user link
userUrl :: Key Account   -- ^ Account ID
        -> Key User      -- ^ User ID
        -> BS.ByteString -- ^ Returns a link to the user
userUrl accountId userId = C.pack $ printf "/accounts/%v/users/%v" accountId userId

-- | The API handler for GET /accounts/:account_id/users
listUsersHandler :: AppHandler ()
listUsersHandler = do
    accountId <- requireParam getKeyParam "account_id"
    filterName <- getTextParam "name"
    filterEmail <- getEmailParam "email"
    page <- getPageParam
    users <- selectUsers accountId filterName filterEmail page
    writeJSON users

-- | The API handler for GET /accounts/:account_id/users/:user_id
showUserHandler :: AppHandler ()
showUserHandler = do
    accountId <- requireParam getKeyParam "account_id"
    userId <- requireParam getKeyParam "user_id"
    user <- getUser accountId userId
    writeJSON user

-- | The API handler for POST/PUT /accounts/:account_id/users
createUserHandler :: AppHandler ()
createUserHandler = do
    accountId <- requireParam getKeyParam "account_id"
    params <- getRequestBody
    userId <- newUser accountId params >>= insertUser
    redirect $ userUrl accountId userId

-- | The API handler for POST/PUT /accounts/:account_id/users/:user_id
updateUserHandler :: AppHandler ()
updateUserHandler = do
    accountId <- requireParam getKeyParam "account_id"
    userId <- requireParam getKeyParam "user_id"
    params <- getRequestBody
    _ <- getUser accountId userId
    updateUser userId params
    modifyResponse $ setResponseCode 204

-- | The API handler for DELETE /accounts/:account_id/users/:user_id
deleteUserHandler :: AppHandler ()
deleteUserHandler = do
    accountId <- requireParam getKeyParam "account_id"
    userId <- requireParam getKeyParam "user_id"
    _ <- getUser accountId userId
    deleteUser userId
    modifyResponse $ setResponseCode 204
