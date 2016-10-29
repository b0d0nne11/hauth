{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains API handlers pertaining to the 'User' object
module Handlers.User (
    -- * API handlers
    listUsersApi,
    showUserApi,
    createUserApi,
    updateUserApi,
    deleteUserApi,
    -- * Utility functions
    userUrl
  ) where

import           Data.Aeson            (ToJSON, object, toJSON, (.=))
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import           Database.Persist      (Entity (..))
import           Snap.Core             (modifyResponse, redirect,
                                        setResponseCode)
import           Text.Printf           (printf)

import           Application           (AppHandler)
import           Helpers               (getEmailParam, getKeyParam,
                                        getPageParam, getRequestBody,
                                        getTextParam, requireParam, writeJSON)
import           Models.User           (CreateParams, UpdateParams, deleteUser,
                                        getDomainUser, insertUser, newUser,
                                        selectUsers, updateUser)
import           Schema

instance ToJSON (Entity User) where
    toJSON  (Entity key (User domainId name email _)) =
        object [ "id"         .= key
               , "domain_id"  .= domainId
               , "name"       .= name
               , "email"      .= email
               ]

-- | Get a user link
userUrl :: Entity User    -- ^ User
        -> BS.ByteString  -- ^ Returns a link to the user
userUrl (Entity userId user) = C.pack $ printf "/domains/%v/users/%v" (userDomainId user) userId

-- | The API handler for GET /domains/:domain_id/users
listUsersApi :: AppHandler ()
listUsersApi = do
    domainId <- requireParam getKeyParam "domain_id"
    filterName <- getTextParam "name"
    filterEmail <- getEmailParam "email"
    filterAccountId <- getKeyParam "account_id"
    page <- getPageParam
    users <- selectUsers domainId filterName filterEmail filterAccountId page
    writeJSON (users :: [Entity User])

-- | The API handler for GET /domains/:domain_id/users/:user_id
showUserApi :: AppHandler ()
showUserApi = do
    domainId <- requireParam getKeyParam "domain_id"
    userId <- requireParam getKeyParam "user_id"
    user <- getDomainUser domainId userId
    writeJSON (user :: Entity User)

-- | The API handler for POST/PUT /domains/:domain_id/users
createUserApi :: AppHandler ()
createUserApi = do
    domainId <- requireParam getKeyParam "domain_id"
    params <- (getRequestBody :: AppHandler CreateParams)
    user <- newUser domainId params
    userId <- insertUser user
    redirect $ userUrl $ Entity userId user

-- | The API handler for POST/PUT /domains/:domain_id/users/:user_id
updateUserApi :: AppHandler ()
updateUserApi = do
    domainId <- requireParam getKeyParam "domain_id"
    userId <- requireParam getKeyParam "user_id"
    params <- (getRequestBody :: AppHandler UpdateParams)
    _ <- getDomainUser domainId userId
    updateUser userId params
    modifyResponse $ setResponseCode 204

-- | The API handler for DELETE /domains/:domain_id/users/:user_id
deleteUserApi :: AppHandler ()
deleteUserApi = do
    domainId <- requireParam getKeyParam "domain_id"
    userId <- requireParam getKeyParam "user_id"
    _ <- getDomainUser domainId userId
    deleteUser userId
    modifyResponse $ setResponseCode 204
