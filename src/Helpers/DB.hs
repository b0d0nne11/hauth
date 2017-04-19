{-# LANGUAGE OverloadedStrings #-}

-- | This module contains helpful utility functions
module Helpers.DB (
    -- * DB helpers
    resetDB,
  ) where

import           Database.Persist.Sql    (rawExecute)
import           Snap.Snaplet.Persistent (runPersist)

import           Application             (AppHandler)
import           Types                   ()

-- | Remove all the records from the database. Meant for testing *only*.
resetDB :: AppHandler ()
resetDB =
    runPersist $ rawExecute "TRUNCATE TABLE account RESTART IDENTITY CASCADE;" []
