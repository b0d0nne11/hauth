{-# LANGUAGE OverloadedStrings #-}

-- | This module contains API handlers pertaining to the 'Token' object
module Handlers.Token (
    -- * API handlers
    authenticateTokenHandler,
    validateTokenHandler
  ) where

import           Control.Lens         ((^.))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Data.Text.Read       as T
import           Database.Persist     (Entity (..))
import           Database.Persist.Sql (toSqlKey)
import           Snap.Core            (getParam)
import qualified Snap.Snaplet.JWT     as JWT
import           Text.Printf          (printf)

import           Application          (AppHandler)
import           Handlers.User        ()
import           Helpers.Crypto       (verify)
import           Helpers.Params       (getKeyParam, getRequestBody,
                                       requireParam)
import           Helpers.Responses    (notFound, serverError, writeJSON,
                                       writeKV)
import           Models.Token         (AuthParams (..))
import           Models.User          (findUserByName, getUser)
import           Schema

-- | The API handler for POST/PUT /accounts/:account_id/tokens
authenticateTokenHandler :: AppHandler ()
authenticateTokenHandler = do
    accountId <- requireParam getKeyParam "account_id"
    (AuthParams name pass) <- getRequestBody
    (Entity userId user) <- findUserByName accountId name
    if verify pass (userPassword user)
        then do
            eToken <- JWT.issueToken $ T.pack $ printf "%v" userId
            case eToken of
                Left e -> serverError $ printf "failed to authenticate: %v: account_id=%v user_id=%v" e accountId userId
                Right token -> writeKV "token" $ T.decodeUtf8 $ LBS.toStrict token
        else
            notFound $ printf "failed to authenticate: invalid password: account_id=%v user_id=%v" accountId userId

-- | The API handler for GET /accounts/:account_id/tokens/:token
validateTokenHandler :: AppHandler ()
validateTokenHandler = do
    accountId <- requireParam getKeyParam "account_id"
    token <- requireParam getParam "token"
    eClaims <- JWT.parseToken $ LBS.fromStrict token
    case eClaims of
        Left e -> notFound $ printf "failed to validate: invalid token: %v: account_id=%v" e accountId
        Right claims ->
            case getSub claims >>= subToString >>= subToDecimal >>= subToKey of
                Left e -> serverError $ printf "failed to validate: invalid claim set: %v: account_id=%v claims=%v" (T.unpack e) accountId (show claims)
                Right userId -> do
                    user <- getUser accountId userId
                    writeJSON user
  where
    getSub a = maybe (Left "claim set missing subject") Right $ a ^. JWT.claimSub
    subToString = maybe (Left "claim set subject is not a string") Right . JWT.getString
    subToDecimal a = case T.decimal a of
        Right (int, "") -> Right int
        _ -> Left "claim set subject is not an integer"
    subToKey = Right . toSqlKey
