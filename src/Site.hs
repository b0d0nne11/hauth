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
import           Handlers.Token
import           Handlers.User
import           Schema

-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("/accounts",
            method GET listAccountsHandler
            <|> methods [POST, PUT] createAccountHandler)
         , ("/accounts/:account_id",
            method GET showAccountHandler
            <|> methods [POST, PUT] updateAccountHandler
            <|> method DELETE deleteAccountHandler)
         , ("/accounts/:account_id/users",
            method GET listUsersHandler
            <|> methods [POST, PUT] createUserHandler)
         , ("/accounts/:account_id/users/:user_id",
            method GET showUserHandler
            <|> methods [POST, PUT] updateUserHandler
            <|> method DELETE deleteUserHandler)
         , ("/accounts/:account_id/tokens",
            method POST authenticateTokenHandler)
         , ("/accounts/:account_id/tokens/:token",
            method GET validateTokenHandler)
         , ("", serveDirectory "static")
         ]

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "hauth" "A slightly less horrible identity API." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    j <- nestSnaplet "" jwt jwtInit
    p <- nestSnaplet "" persist $ initPersist (runMigrationUnsafe migrateAll)
    wrapCORS
    addRoutes routes
    return $ App h j p
