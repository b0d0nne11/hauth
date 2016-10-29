{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- | This module defines the DB interface and schema
module Schema where

import           Crypto.Scrypt           (EncryptedPass)
import           Data.Text               (Text)
import           Database.Persist        (Entity (..), Key, ToBackendKey,
                                          Unique, get, getBy)
import           Database.Persist.Sql    (SqlBackend, rawExecute)
import           Database.Persist.TH     (mkMigrate, mkPersist,
                                          persistLowerCase, share, sqlSettings)
import           Snap.Snaplet.Persistent (runPersist)
import           Text.Email.Parser       (EmailAddress)
import           Text.Printf             (printf)

import           Application             (AppHandler)
import           Helpers                 (notFound)
import           Types                   ()

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Domain
    name Text
    deriving Show Eq
Account
    domainId DomainId
    name Text
    UniqueAccountName domainId name
    deriving Show Eq
User
    domainId DomainId
    name Text
    email EmailAddress
    encryptedPassword EncryptedPass
    UniqueUserName domainId name
    deriving Show Eq
AccountUser
    accountId AccountId
    userId UserId
    Primary accountId userId
    deriving Show Eq
|]

-- | Get a record by identifier or finish with a 404 Not Found
get404 :: (ToBackendKey SqlBackend val) => Key val -> AppHandler (Entity val)
get404 key = do
    mRow <- runPersist $ get key
    case mRow of
        Nothing  -> notFound $ printf "no record found for key $v" key
        Just row -> return (Entity key row)

-- | Get a record by unique key or finish with a 404 Not Found
getBy404 :: (ToBackendKey SqlBackend val) => Unique val -> AppHandler (Entity val)
getBy404 constraint = do
    mEntity <- runPersist $ getBy constraint
    case mEntity of
        Nothing     -> notFound $ printf "no record found for constraint %v" constraint
        Just entity -> return entity

-- | Remove all the records from the database. Meant for testing *only*.
resetDB :: AppHandler ()
resetDB =
    runPersist $ rawExecute "TRUNCATE TABLE domain RESTART IDENTITY CASCADE;" []
