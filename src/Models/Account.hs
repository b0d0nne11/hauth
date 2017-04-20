{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains DB accessors pertaining to the 'Account' object
module Models.Account (
    -- * Parameters
    CreateParams (..),
    UpdateParams (..),
    -- * DB accessors
    newAccount,
    selectAccounts,
    getAccount,
    insertAccount,
    updateAccount,
    deleteAccount
  ) where

import           Control.Applicative     (empty)
import           Data.Aeson              (FromJSON, ToJSON, object, parseJSON,
                                          toJSON, (.:), (.:?), (.=))
import           Data.Aeson.Types        (Value (..))
import           Data.Maybe              (catMaybes)
import qualified Data.Text               as T
import           Database.Esqueleto      (asc, delete, desc, from, insertUnique,
                                          limit, orderBy, select, set, update,
                                          val, where_, (&&.), (<.), (=.), (==.),
                                          (>.), (^.))
import           Database.Persist        (Entity (..), Key)
import           Snap.Snaplet.Persistent (runPersist)
import           Text.Printf             (printf)

import           Application             (AppHandler)
import           Helpers.Responses       (badRequest, notFound)
import           Models.Page             (Page (..))
import           Schema

instance ToJSON (Entity Account) where
    toJSON  (Entity key (Account name)) =
        object [ "id"   .= key
               , "name" .= name
               ]

-- | Create parameters
data CreateParams = CreateParams
    { _createParamsName :: T.Text }

instance FromJSON CreateParams where
    parseJSON (Object v) = CreateParams <$> v .: "name"
    parseJSON _ = empty

instance ToJSON CreateParams where
    toJSON (CreateParams name) =
        object [ "name" .= name ]

-- | Update parameters
data UpdateParams = UpdateParams
    { _updateParamsName :: Maybe T.Text }

instance FromJSON UpdateParams where
    parseJSON (Object v) = UpdateParams <$> v .:? "name"
    parseJSON _ = empty

instance ToJSON UpdateParams where
    toJSON (UpdateParams name) =
        object [ "name" .= name ]

-- | Create a new account
newAccount :: CreateParams       -- ^ Account create parameters
           -> AppHandler Account -- ^ Returns a new account
newAccount (CreateParams name) = return $ Account name

-- | Get an account by ID or finish with a 404 Not Found
getAccount :: Key Account                 -- ^ Account ID
           -> AppHandler (Entity Account) -- ^ Returns an account
getAccount accountId = do
    accounts <- runPersist $ select $
        from $ \a -> do
            where_ $ a ^. AccountId ==. val accountId
            return a
    case accounts of
        [a] -> return a
        _ -> notFound $ printf "account not found: account_id=%v" accountId

-- | Get a list of accounts
selectAccounts :: Maybe T.Text                -- ^ Optionally filter by account name
               -> Page Account                -- ^ Pagination parameters
               -> AppHandler [Entity Account] -- ^ Returns a list of accounts
selectAccounts filterName page =
    runPersist $ select $
        from $ \a -> do
            where_ $ foldl (&&.)
                (val True)
                (catMaybes [ (a ^. AccountName ==.) . val <$> filterName
                           , (a ^. AccountId    <.) . val <$> pageBefore page
                           , (a ^. AccountId    >.) . val <$> pageAfter page
                           ]
                )
            if pageOrder page == "asc"
                then orderBy [asc  (a ^. AccountId)]
                else orderBy [desc (a ^. AccountId)]
            limit $ pageLimit page
            return a

-- | Insert an account
insertAccount :: Account                  -- ^ Account to insert
              -> AppHandler (Key Account) -- ^ Returns an account ID
insertAccount account = do
    mAccountId <- runPersist $ insertUnique account
    maybe (badRequest "account fails unique constraints") return mAccountId

-- | Update an account
updateAccount :: Key Account   -- ^ Account ID
              -> UpdateParams  -- ^ Account update parameters
              -> AppHandler ()
updateAccount _ (UpdateParams Nothing) = return ()
updateAccount accountId (UpdateParams mName) =
    runPersist $ update $ \a -> do
        set a $ catMaybes [ (AccountName =.) . val <$> mName ]
        where_ $ a ^. AccountId ==. val accountId

-- | Delete an account
deleteAccount :: Key Account -- ^ Account ID
              -> AppHandler ()
deleteAccount accountId =
    runPersist $ delete $
        from $ \a ->
            where_ $ a ^. AccountId ==. val accountId
