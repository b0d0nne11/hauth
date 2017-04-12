{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains DB accessors pertaining to the 'User' object
module Models.User (
    -- * DB accessors
    CreateParams (..),
    UpdateParams (..),
    newUser,
    selectUsers,
    getDomainUser,
    insertUser,
    updateUser,
    deleteUser
  ) where

import           Control.Applicative     (empty)
import           Crypto.Scrypt           (Pass)
import           Data.Aeson              (FromJSON, ToJSON, object, parseJSON,
                                          toJSON, (.:), (.:?), (.=))
import           Data.Aeson.Types        (Value (..))
import           Data.Maybe              (catMaybes)
import qualified Data.Text               as T
import           Database.Esqueleto      (LeftOuterJoin (..), asc, delete, desc,
                                          distinct, from, insert, limit, on,
                                          orderBy, select, set, update, val,
                                          where_, (&&.), (<.), (=.), (==.),
                                          (>.), (^.))
import           Database.Persist        (Entity (..), Key)
import           Snap.Snaplet.Persistent (runPersist)
import           Text.Email.Parser       (EmailAddress)
import           Text.Printf             (printf)

import           Application             (AppHandler)
import           Helpers                 (Page (..), encrypt, notFound)
import           Schema

data CreateParams = CreateParams
    { _createParamsName  :: T.Text
    , _createParamsEmail :: EmailAddress
    , _createParamsPass  :: Pass
    }

instance FromJSON CreateParams where
    parseJSON (Object v) = CreateParams <$> v .: "name"
                                        <*> v .: "email"
                                        <*> v .: "password"
    parseJSON _ = empty

instance ToJSON CreateParams where
    toJSON (CreateParams name email pass) =
        object [ "name"     .= name
               , "email"    .= email
               , "password" .= pass
               ]

data UpdateParams = UpdateParams
    { updateParamsName  :: Maybe T.Text
    , updateParamsEmail :: Maybe EmailAddress
    , updateParamsPass  :: Maybe Pass
    }

instance FromJSON UpdateParams where
    parseJSON (Object v) = UpdateParams <$> v .:? "name"
                                        <*> v .:? "email"
                                        <*> v .:? "password"
    parseJSON _ = empty

instance ToJSON UpdateParams where
    toJSON (UpdateParams name email pass) =
        object [ "name"     .= name
               , "email"    .= email
               , "password" .= pass
               ]

-- | Create a new user
newUser :: Key Domain      -- ^ User domain ID
        -> CreateParams    -- ^ User create parameters
        -> AppHandler User -- ^ Returns a new user
newUser domainId (CreateParams name email pass) = do
    pass' <- encrypt pass
    return $ User domainId name email pass'

-- | Get a user by ID or finish with a 404 Not Found
getDomainUser :: Key Domain               -- ^ User domain ID
              -> Key User                 -- ^ User ID
              -> AppHandler (Entity User) -- ^ Returns a user
getDomainUser domainId userId = do
    users <- runPersist $ select $
        from $ \u -> do
            where_ $ u ^. UserDomainId ==. val domainId
                 &&. u ^. UserId       ==. val userId
            return u
    case users of
        [u] -> return u
        _ -> notFound $ printf "user %v in domain %v not found" userId domainId

-- | Get a list of users
selectUsers :: Key Domain                 -- ^ Filter by user domain ID
            -> Maybe T.Text               -- ^ Optionally filter by user name
            -> Maybe EmailAddress         -- ^ Optionally filter by user email address
            -> Maybe (Key Account)        -- ^ Optionally filter by user account ID
            -> Page User                  -- ^ Pagination parameters
            -> AppHandler ([Entity User]) -- ^ Returns a list of users
selectUsers domainId filterName filterEmail filterAccountId page = do
    users <- runPersist $ select $ distinct $
        from $ \(u `LeftOuterJoin` au `LeftOuterJoin` a) -> do
            on $ a ^. AccountId ==. au ^. AccountUserAccountId
            on $ u ^. UserId    ==. au ^. AccountUserUserId
            where_ $ foldl (&&.)
                (u ^. UserDomainId ==. val domainId)
                (catMaybes [ (u ^. UserName  ==.) . val <$> filterName
                           , (u ^. UserEmail ==.) . val <$> filterEmail
                           , (a ^. AccountId ==.) . val <$> filterAccountId
                           , (u ^. UserId     <.) . val <$> pageBefore page
                           , (u ^. UserId     >.) . val <$> pageAfter page
                           ]
                )
            if pageOrder page == "asc"
                then orderBy [asc  (u ^. UserId)]
                else orderBy [desc (u ^. UserId)]
            limit $ pageLimit page
            return u
    return users

-- | Insert a user record
insertUser :: User                  -- ^ User to insert
           -> AppHandler (Key User) -- ^ Returns a user ID
insertUser user = do
    runPersist $ insert user

-- | Update a user record
updateUser :: Key User     -- ^ User ID
           -> UpdateParams -- ^ User update parameters
           -> AppHandler ()
updateUser _ (UpdateParams Nothing Nothing Nothing) = return ()
updateUser userId params = do
    pass' <- mapM encrypt $ updateParamsPass params
    runPersist $ update $ \u -> do
        set u $ catMaybes [ (UserName              =.) . val <$> updateParamsName params
                          , (UserEmail             =.) . val <$> updateParamsEmail params
                          , (UserEncryptedPassword =.) . val <$> pass'
                          ]
        where_ $ u ^. UserId ==. val userId

-- | Delete a user record
deleteUser :: Key User -- ^ User ID
           -> AppHandler ()
deleteUser userId = do
    runPersist $ delete $ from $ \au -> do
        where_ $ au ^. AccountUserUserId ==. val userId
    runPersist $ delete $ from $ \u -> do
        where_ $ u ^. UserId ==. val userId
