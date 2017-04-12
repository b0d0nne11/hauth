{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains DB accessors pertaining to the 'Account' object
module Models.Account (
    -- * DB accessors
    CreateParams (..),
    UpdateParams (..),
    newAccount,
    selectAccounts,
    getDomainAccount,
    insertAccount,
    updateAccount,
    updateAccountUsers,
    deleteAccount
  ) where

import           Control.Applicative     (empty)
import           Control.Monad           (forM_)
import           Data.Aeson              (FromJSON, ToJSON, object, parseJSON,
                                          toJSON, (.:), (.:?), (.=))
import           Data.Aeson.Types        (Value (..))
import           Data.Maybe              (catMaybes, fromMaybe)
import qualified Data.Text               as T
import           Database.Esqueleto      (LeftOuterJoin (..), asc, delete, desc,
                                          distinct, from, insert, limit, on,
                                          orderBy, select, set, update, val,
                                          where_, (&&.), (<.), (=.), (==.),
                                          (>.), (^.))
import           Database.Persist        (Entity (..), Key)
import           Snap.Snaplet.Persistent (runPersist)
import           Text.Printf             (printf)

import           Application             (AppHandler)
import           Helpers                 (Page (..), notFound)
import           Schema

data CreateParams = CreateParams
    { _createParamsName :: T.Text }

instance FromJSON CreateParams where
    parseJSON (Object v) = CreateParams <$> v .: "name"
    parseJSON _ = empty

instance ToJSON CreateParams where
    toJSON (CreateParams name) =
        object [ "name" .= name ]

data UpdateParams = UpdateParams
    { updateParamsName      :: Maybe T.Text
    , updateParamsLinkUsers :: Maybe [Key User]
    , updateParamsDropUsers :: Maybe [Key User]
    }

instance FromJSON UpdateParams where
    parseJSON (Object v) = UpdateParams <$> v .:? "name"
                                        <*> v .:? "link_users"
                                        <*> v .:? "drop_users"
    parseJSON _ = empty

instance ToJSON UpdateParams where
    toJSON (UpdateParams name linkUsers dropUsers) =
        object [ "name"       .= name
               , "link_users" .= linkUsers
               , "drop_users" .= dropUsers
               ]

-- | Create a new account
newAccount :: Key Domain   -- ^ Account domain ID
           -> CreateParams -- ^ Account create parameters
           -> Account      -- ^ Returns a new account
newAccount domainId (CreateParams name) =
    Account domainId name

-- | Get an account by ID or finish with a 404 Not Found
getDomainAccount :: Key Domain                  -- ^ Account domain ID
                 -> Key Account                 -- ^ Account ID
                 -> AppHandler (Entity Account) -- ^ Returns an account
getDomainAccount domainId accountId = do
    accounts <- runPersist $ select $
        from $ \a -> do
            where_ $ a ^. AccountDomainId ==. val domainId
                 &&. a ^. AccountId       ==. val accountId
            return a
    case accounts of
        [a] -> return a
        _ -> notFound $ printf "account %v in domain %v not found" accountId domainId

-- | Get a list of accounts
selectAccounts :: Key Domain                  -- ^ Filter by account domain ID
               -> Maybe T.Text                -- ^ Optionally filter by account name
               -> Maybe (Key User)            -- ^ Optionally filter by account user ID
               -> Page Account                -- ^ Pagination parameters
               -> AppHandler [Entity Account] -- ^ Returns a list of accounts
selectAccounts domainId filterName filterUserId page = do
    accounts <- runPersist $ select $ distinct $
        from $ \(a `LeftOuterJoin` au `LeftOuterJoin` u) -> do
            on $ u ^. UserId    ==. au ^. AccountUserUserId
            on $ a ^. AccountId ==. au ^. AccountUserAccountId
            where_ $ foldl (&&.)
                (a ^. AccountDomainId ==. val domainId)
                (catMaybes [ (a ^. AccountName ==.) . val <$> filterName
                           , (u ^. UserId      ==.) . val <$> filterUserId
                           , (a ^. AccountId    <.) . val <$> pageBefore page
                           , (a ^. AccountId    >.) . val <$> pageAfter page
                           ]
                )
            if pageOrder page == "asc"
                then orderBy [asc  (a ^. AccountId)]
                else orderBy [desc (a ^. AccountId)]
            limit $ pageLimit page
            return a
    return accounts

-- | Insert an account record
insertAccount :: Account                  -- ^ Account to insert
              -> AppHandler (Key Account) -- ^ Returns an account ID
insertAccount account = do
    runPersist $ insert account

-- | Update an account record
updateAccount :: Key Account   -- ^ Account ID
              -> UpdateParams  -- ^ Account update parameters
              -> AppHandler ()
updateAccount _ (UpdateParams Nothing _ _) = return ()
updateAccount accountId params = do
    runPersist $ update $ \a -> do
        set a $ catMaybes [ (AccountName =.) . val <$> updateParamsName params ]
        where_ $ a ^. AccountId ==. val accountId

-- | Update account user membership
updateAccountUsers :: Key Account   -- ^ Account ID
                   -> UpdateParams  -- ^ Account update parameters
                   -> AppHandler ()
updateAccountUsers accountId params = do
    forM_ (fromMaybe [] $ updateParamsLinkUsers params) $ \userId -> do
        rows <- runPersist $ select $ from $ \au -> do
            where_ $ au ^. AccountUserAccountId ==. val accountId
                 &&. au ^. AccountUserUserId    ==. val userId
            return au
        case rows of
            [Entity key _] -> return key
            _ -> runPersist $ insert $ AccountUser accountId userId
    forM_ (fromMaybe [] $ updateParamsDropUsers params) $ \userId -> do
        runPersist $ delete $ from $ \au -> do
            where_ $ au ^. AccountUserAccountId ==. val accountId
                 &&. au ^. AccountUserUserId    ==. val userId

-- | Delete an account record
deleteAccount :: Key Account -- ^ Account ID
              -> AppHandler ()
deleteAccount accountId = do
    runPersist $ delete $ from $ \au -> do
        where_ $ au ^. AccountUserAccountId ==. val accountId
    runPersist $ delete $ from $ \a -> do
        where_ $ a ^. AccountId ==. val accountId
