{-# LANGUAGE OverloadedStrings #-}

-- | This module contains API handlers pertaining to the 'Token' object
module Handlers.Token (
    -- * API handlers
    authenticateApi,
    validateApi
  ) where

import           Control.Lens         ((^.))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import           Database.Persist     (Entity (..), Key)
import           Database.Persist.Sql (toSqlKey)
import           Snap.Core            (getParam)
import qualified Snap.Snaplet.JWT     as JWT
import           Text.Printf          (printf)

import           Application          (AppHandler)
import           Handlers.User        ()
import           Helpers              (getKeyParam, getRequestBody, notFound,
                                       requireParam, serverError, verify,
                                       writeJSON, writeKV)
import           Models.Token         (AuthParams (..))
import           Schema

-- | The API handler for POST/PUT /domains/:domain_id/tokens
authenticateApi :: AppHandler ()
authenticateApi = do
    domainId <- requireParam getKeyParam "domain_id"
    (AuthParams name pass) <- getRequestBody
    (Entity userId user) <- getBy404 $ UniqueUserName domainId name
    let (isVerified, _) = verify pass (userEncryptedPassword user)
    if isVerified
        then do
            token <- JWT.issueToken $ T.pack $ printf "%v" userId
            tokenResponse token
        else
            notFound $ printf "failed to authenticate: invalid password for user %v" userId

tokenResponse :: Either JWT.Error LBS.ByteString -> AppHandler ()
tokenResponse (Left e) =
    serverError $ printf "failed to issue token: %v" e
tokenResponse (Right token) =
    writeKV "token" $ T.decodeUtf8 $ LBS.toStrict token

-- | The API handler for GET /domains/:domain_id/tokens/:token
validateApi :: AppHandler ()
validateApi = do
    token <- requireParam getParam "token"
    claims <- JWT.parseToken (LBS.fromStrict token)
    claimsResponse claims

claimsResponse :: Either JWT.Error JWT.ClaimsSet -> AppHandler ()
claimsResponse (Left e) =
    notFound $ printf "failed to parse claim set: %v" e
claimsResponse (Right claims) = do
    userId <- claimsUserId claims
    user <- get404 userId
    writeJSON (user :: Entity User)

claimsUserId :: JWT.ClaimsSet -> AppHandler (Key User)
claimsUserId claims =
    case claims ^. JWT.claimSub of
        Nothing -> serverError "failed to extract claim set subject"
        Just sub -> case JWT.getString sub of
            Nothing -> serverError "claim set subject is not a string"
            Just text -> return . toSqlKey . read . T.unpack $ text
