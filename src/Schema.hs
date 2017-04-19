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

import           Crypto.Scrypt       (EncryptedPass)
import           Data.Text           (Text)
import           Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase,
                                      share, sqlSettings)
import           Text.Email.Parser   (EmailAddress)

import           Types               ()

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Account
    name Text
    UniqueAccountName name
    deriving Show Eq
User
    accountId AccountId
    name Text
    email EmailAddress
    password EncryptedPass
    UniqueUserName accountId name
    deriving Show Eq
|]
