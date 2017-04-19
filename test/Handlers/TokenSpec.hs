{-# LANGUAGE OverloadedStrings #-}

module Handlers.TokenSpec (
    spec
) where

import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe                 (fromJust)
import qualified Data.Text                  as T
import           Database.Persist.Class     (insert)
import           Database.Persist.Sql       (runMigrationUnsafe)
import           Snap.Core                  (route)
import           Snap.Snaplet               (with)
import qualified Snap.Snaplet.JWT           as JWT
import           Snap.Snaplet.Persistent    (runPersist)
import           Test.Hspec                 (Spec, describe, it)
import           Test.Hspec.Snap            (TestResponse (..), afterEval,
                                             beforeEval, eval, get, postJson,
                                             should200, should404, shouldEqual,
                                             snap)
import           Text.Email.Validate        (emailAddress)

import           Application                (AppHandler, jwt)
import           Helpers.Crypto             (Pass (..))
import           Helpers.DB                 (resetDB)
import           Models.Account             (newAccount)
import qualified Models.Account             as Account
import qualified Models.Token               as Token
import           Models.User                (newUser)
import qualified Models.User                as User
import           Schema                     (migrateAll)
import           Site                       (app, routes)

fixtures :: AppHandler ()
fixtures = do
    runPersist $ runMigrationUnsafe migrateAll
    a1 <- runPersist . insert =<< newAccount (Account.CreateParams "account1")
    _  <- runPersist . insert =<< newUser a1 (User.CreateParams "user1" (fromJust $ emailAddress "user1@example.com") (Pass "password"))
    return ()

spec :: Spec
spec = snap (route routes) app $ beforeEval fixtures $ afterEval resetDB $ do

    describe "authenticate" $ do
        it "authenticates a user and returns a token and 200 OK" $
            postJson "/accounts/1/tokens" (Token.AuthParams "user1" (Pass "password")) >>= should200
        it "returns a 404 Not Found when user doesnt exist" $
            postJson "/accounts/1/tokens" (Token.AuthParams "user4" (Pass "password")) >>= should404
        it "returns a 404 Not Found when password doesnt match" $
            postJson "/accounts/1/tokens" (Token.AuthParams "user1" (Pass "invalid")) >>= should404

    describe "validate" $
        it "validates a token and returns a user record and 200 OK" $ do
            Right token <- eval $ with jwt $ JWT.issueToken "1"
            let tokenUrl = T.pack $ "/accounts/1/tokens/" ++ C.unpack token
            get tokenUrl >>= (`shouldEqual` Json 200 "{\"email\":\"user1@example.com\",\"name\":\"user1\",\"id\":1,\"account_id\":1}")
