{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains DB accessors pertaining to the 'User' object
module Models.User (
    -- * Parameters
    CreateParams (..),
    UpdateParams (..),
    -- * DB accessors
    newUser,
    selectUsers,
    getUser,
    findUserByName,
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
import           Database.Esqueleto      (asc, delete, desc, from, insert,
                                          limit, orderBy, select, set, update,
                                          val, where_, (&&.), (<.), (=.), (==.),
                                          (>.), (^.))
import           Database.Persist        (Entity (..), Key)
import           Snap.Snaplet.Persistent (runPersist)
import           Text.Email.Parser       (EmailAddress)
import           Text.Printf             (printf)

import           Application             (AppHandler)
import           Helpers.Crypto          (encrypt)
import           Helpers.Responses       (notFound)
import           Models.Page             (Page (..))
import           Schema

instance ToJSON (Entity User) where
    toJSON  (Entity key (User accountId name email _)) =
        object [ "id"         .= key
               , "account_id" .= accountId
               , "name"       .= name
               , "email"      .= email
               ]

-- | Create parameters
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

-- | Update parameters
data UpdateParams = UpdateParams
    { _updateParamsName  :: Maybe T.Text
    , _updateParamsEmail :: Maybe EmailAddress
    , _updateParamsPass  :: Maybe Pass
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
newUser :: Key Account     -- ^ User account ID
        -> CreateParams    -- ^ User create parameters
        -> AppHandler User -- ^ Returns a new user
newUser accountId (CreateParams name email pass) = do
    pass' <- encrypt pass
    return $ User accountId name email pass'

-- | Get a user by ID or finish with a 404 Not Found
getUser :: Key Account              -- ^ Account ID
        -> Key User                 -- ^ User ID
        -> AppHandler (Entity User) -- ^ Returns a user
getUser accountId userId = do
    users <- runPersist $ select $
        from $ \u -> do
            where_ $ u ^. UserAccountId ==. val accountId
                 &&. u ^. UserId        ==. val userId
            return u
    case users of
        [u] -> return u
        _ -> notFound $ printf "user not found: account_id=%v user_id=%v" accountId userId

-- | Find a user by name or finish with a 404 Not Found
findUserByName :: Key Account              -- ^ Account ID
               -> T.Text                   -- ^ User name
               -> AppHandler (Entity User) -- ^ Returns a user
findUserByName accountId name = do
    users <- runPersist $ select $
        from $ \u -> do
            where_ $ u ^. UserAccountId ==. val accountId
                 &&. u ^. UserName      ==. val name
            return u
    case users of
        [u] -> return u
        _ -> notFound $ printf "user not found: account_id=%v user_name=%v" accountId name

-- | Get a list of users
selectUsers :: Key Account              -- ^ Filter by user domain ID
            -> Maybe T.Text             -- ^ Optionally filter by user name
            -> Maybe EmailAddress       -- ^ Optionally filter by user email address
            -> Page User                -- ^ Pagination parameters
            -> AppHandler [Entity User] -- ^ Returns a list of users
selectUsers accountId filterName filterEmail page =
    runPersist $ select $
        from $ \u -> do
            where_ $ foldl (&&.)
                (u ^. UserAccountId ==. val accountId)
                (catMaybes [ (u ^. UserName  ==.) . val <$> filterName
                           , (u ^. UserEmail ==.) . val <$> filterEmail
                           , (u ^. UserId     <.) . val <$> pageBefore page
                           , (u ^. UserId     >.) . val <$> pageAfter page
                           ]
                )
            if pageOrder page == "asc"
                then orderBy [asc  (u ^. UserId)]
                else orderBy [desc (u ^. UserId)]
            limit $ pageLimit page
            return u

-- | Insert a user
insertUser :: User                  -- ^ User to insert
           -> AppHandler (Key User) -- ^ Returns a user ID
insertUser user =
    runPersist $ insert user

-- | Update a user
updateUser :: Key User     -- ^ User ID
           -> UpdateParams -- ^ User update parameters
           -> AppHandler ()
updateUser _ (UpdateParams Nothing Nothing Nothing) = return ()
updateUser userId (UpdateParams mName mEmail mPass) = do
    mPass' <- mapM encrypt mPass
    runPersist $ update $ \u -> do
        set u $ catMaybes [ (UserName     =.) . val <$> mName
                          , (UserEmail    =.) . val <$> mEmail
                          , (UserPassword =.) . val <$> mPass'
                          ]
        where_ $ u ^. UserId ==. val userId

-- | Delete a user
deleteUser :: Key User -- ^ User ID
           -> AppHandler ()
deleteUser userId =
    runPersist $ delete $
        from $ \u ->
            where_ $ u ^. UserId ==. val userId
