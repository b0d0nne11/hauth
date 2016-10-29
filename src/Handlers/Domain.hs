{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains API handlers pertaining to the 'Domain' object
module Handlers.Domain (
    -- * API handlers
    listDomainsApi,
    showDomainApi,
    createDomainApi,
    updateDomainApi,
    deleteDomainApi,
    -- * Utility functions
    domainUrl
  ) where

import           Data.Aeson            (ToJSON, object, toJSON, (.=))
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import           Database.Persist      (Entity (..), Key, toBackendKey)
import           Snap.Core             (modifyResponse, redirect,
                                        setResponseCode)
import           Text.Printf           (printf)

import           Application           (AppHandler)
import           Helpers               (getKeyParam, getPageParam,
                                        getRequestBody, getTextParam,
                                        requireParam, writeJSON)
import           Models.Domain         (CreateParams, UpdateParams,
                                        deleteDomain, insertDomain, newDomain,
                                        selectDomains, updateDomain)
import           Schema

instance ToJSON (Entity Domain) where
    toJSON  (Entity key (Domain name)) =
        object [ "id"   .= key
               , "name" .= name
               ]

-- | Get a domain link
domainUrl :: Entity Domain -- ^ Domain
          -> BS.ByteString -- ^ Returns a link to the domain
domainUrl (Entity domainId _)= C.pack $ printf "/domains/%v" domainId

-- | The API handler for GET /domains
listDomainsApi :: AppHandler ()
listDomainsApi = do
    filterName <- getTextParam "name"
    page <- getPageParam
    domains <- selectDomains filterName page
    writeJSON (domains :: [Entity Domain])

-- | The API handler for GET /domains/:domain_id
showDomainApi :: AppHandler ()
showDomainApi = do
    domainId <- requireParam getKeyParam "domain_id"
    domain <- get404 $ DomainKey $ toBackendKey (domainId :: Key Domain)
    writeJSON (domain :: Entity Domain)

-- | The API handler for POST/PUT /domains
createDomainApi :: AppHandler ()
createDomainApi = do
    domain <- newDomain <$> (getRequestBody :: AppHandler CreateParams)
    domainId <- insertDomain domain
    redirect $ domainUrl $ Entity domainId domain

-- | The API handler for POST/PUT /domains/:domain_id
updateDomainApi :: AppHandler ()
updateDomainApi = do
    domainId <- requireParam getKeyParam "domain_id"
    params <- (getRequestBody :: AppHandler UpdateParams)
    _ <- get404 $ DomainKey $ toBackendKey (domainId :: Key Domain)
    updateDomain domainId params
    modifyResponse $ setResponseCode 204

-- | The API handler for DELETE /domains/:domain_id
deleteDomainApi :: AppHandler ()
deleteDomainApi = do
    domainId <- requireParam getKeyParam "domain_id"
    _ <- get404 $ DomainKey $ toBackendKey (domainId :: Key Domain)
    deleteDomain domainId
    modifyResponse $ setResponseCode 204
