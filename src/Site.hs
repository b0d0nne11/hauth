{-# LANGUAGE OverloadedStrings #-}

-- | This module is where all the routes and handlers are defined
module Site (
    app,
    routes
  ) where

import           Control.Applicative     ((<|>))
import           Data.ByteString         (ByteString)
import           Data.Monoid             ()
import           Database.Persist.Sql    (runMigrationUnsafe)
import           Snap.Core               (Method (..), method, methods)
import           Snap.CORS               (wrapCORS)
import           Snap.Snaplet            (SnapletInit, addRoutes, makeSnaplet,
                                          nestSnaplet)
import           Snap.Snaplet.Heist      (heistInit)
import           Snap.Snaplet.JWT        (jwtInit)
import           Snap.Snaplet.Persistent (initPersist)
import           Snap.Util.FileServe     (serveDirectory)

import           Application
import           Handlers.Account
import           Handlers.Domain
import           Handlers.Token
import           Handlers.User
import           Schema

-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("/domains",
            method GET listDomainsApi
            <|> methods [POST, PUT] createDomainApi)
         , ("/domains/:domain_id",
            method GET showDomainApi
            <|> methods [POST, PUT] updateDomainApi
            <|> method DELETE deleteDomainApi)
         , ("/domains/:domain_id/accounts",
            method GET listAccountsApi
            <|> methods [POST, PUT] createAccountApi)
         , ("/domains/:domain_id/accounts/:account_id",
            method GET showAccountApi
            <|> methods [POST, PUT] updateAccountApi
            <|> method DELETE deleteAccountApi)
         , ("/domains/:domain_id/users",
            method GET listUsersApi
            <|> methods [POST, PUT] createUserApi)
         , ("/domains/:domain_id/users/:user_id",
            method GET showUserApi
            <|> methods [POST, PUT] updateUserApi
            <|> method DELETE deleteUserApi)
         , ("/domains/:domain_id/tokens",
            method POST authenticateApi)
         , ("/domains/:domain_id/tokens/:token",
            method GET validateApi)
         , ("", serveDirectory "static")
         ]

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    j <- nestSnaplet "jwt" jwt jwtInit
    p <- nestSnaplet "persist" persist $ initPersist (runMigrationUnsafe migrateAll)
    wrapCORS
    addRoutes routes
    return $ App h j p
