{-# LANGUAGE OverloadedStrings #-}

-- | This module contains helpful utility functions
module Helpers.Crypto (
    -- * Crypto helpers
    encrypt,
    verify,
    -- * Re-exports
    Pass(..),
    EncryptedPass(..),
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Crypto.Scrypt          (EncryptedPass (..), Pass (..),
                                         defaultParams, encryptPassIO,
                                         verifyPass)

import           Application            (AppHandler)
import           Types                  ()

-- | Generate an encrypted field from cleartext
encrypt :: Pass -> AppHandler EncryptedPass
encrypt plaintext = liftIO $ encryptPassIO defaultParams plaintext

-- | Verify a cleartext password matches an encrypted field
verify :: Pass -> EncryptedPass -> Bool
verify plaintext ciphertext = fst $ verifyPass defaultParams plaintext ciphertext
