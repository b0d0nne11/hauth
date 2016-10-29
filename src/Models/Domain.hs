{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains DB accessors pertaining to the 'Domain' object
module Models.Domain (
    -- * DB accessors
    CreateParams (..),
    UpdateParams (..),
    newDomain,
    selectDomains,
    insertDomain,
    updateDomain,
    deleteDomain
  ) where

import           Control.Applicative     (empty)
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

import           Application             (AppHandler)
import           Helpers                 (Page (..))
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
    { updateParamsName :: Maybe T.Text }

instance FromJSON UpdateParams where
    parseJSON (Object v) = UpdateParams <$> v .:? "name"
    parseJSON _ = empty

instance ToJSON UpdateParams where
    toJSON (UpdateParams name) =
        object [ "name" .= name ]

-- | Create a new domain
newDomain :: CreateParams -- ^ Domain create parameters
          -> Domain       -- ^ Returns a new domain
newDomain (CreateParams name) =
    Domain name

-- | Get a list of domains
selectDomains :: Maybe T.Text                 -- ^ Optionally filter by domain name
              -> Page Domain                  -- ^ Pagination parameters
              -> AppHandler ([Entity Domain]) -- ^ Returns a list of domains
selectDomains filterName page = do
    domains <- runPersist $ select $
        from $ \d -> do
            where_ $ foldl (&&.) (val True) $ catMaybes $
                [ (d ^. DomainName ==.) <$> val <$> filterName
                , (d ^. DomainId    <.) <$> val <$> pageBefore page
                , (d ^. DomainId    >.) <$> val <$> pageAfter page
                ]
            if pageOrder page == "asc"
                then orderBy [asc  (d ^. DomainId)]
                else orderBy [desc (d ^. DomainId)]
            limit $ pageLimit page
            return d
    return domains

-- | Insert a domain record
insertDomain :: Domain                  -- ^ Domain to insert
             -> AppHandler (Key Domain) -- ^ Returns a domain ID
insertDomain domain = do
    runPersist $ insert domain

-- | Update a domain record
updateDomain :: Key Domain   -- ^ Domain ID
             -> UpdateParams -- ^ Domain update parameters
             -> AppHandler ()
updateDomain _ (UpdateParams Nothing) = return ()
updateDomain domainId params = do
    runPersist $ update $ \d -> do
        set d $ catMaybes $
            [ (DomainName =.) <$> val <$> updateParamsName params ]
        where_ $ d ^. DomainId ==. val domainId

-- | Delete a domain record
deleteDomain :: Key Domain -- ^ Domain ID
             -> AppHandler ()
deleteDomain domainId = do
    runPersist $ delete $ from $ \d -> do
        where_ $ d ^. DomainId ==. val domainId
