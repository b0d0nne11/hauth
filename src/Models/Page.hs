{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}

-- | This module contains types pertaining to the 'Page' object
module Models.Page (
    Page (..)
  ) where

import           Data.ByteString      (ByteString)
import           Data.Int             (Int64)
import           Database.Persist     (Key, ToBackendKey)
import           Database.Persist.Sql (SqlBackend)

import           Schema               ()

-- | Page
data Page record = ToBackendKey SqlBackend record =>
    Page { pageOrder  :: ByteString
         , pageLimit  :: Int64
         , pageBefore :: Maybe (Key record)
         , pageAfter  :: Maybe (Key record)
         }
