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
import           Helpers                    (Pass (..))
import           Models.Account             (newAccount)
import qualified Models.Account             as Account
import           Models.Domain              (newDomain)
import qualified Models.Domain              as Domain
import qualified Models.Token               as Token
import           Models.User                (newUser)
import qualified Models.User                as User
import           Schema                     (migrateAll, resetDB)
import           Site                       (app, routes)

fixtures :: AppHandler ()
fixtures = do
    runPersist $ runMigrationUnsafe migrateAll
    d1 <- runPersist . insert $ newDomain (Domain.CreateParams "domain1")
    _  <- runPersist . insert $ newAccount d1 (Account.CreateParams "account1")
    _  <- runPersist . insert =<< newUser d1 (User.CreateParams "user1" (fromJust $ emailAddress "user1@example.com") (Pass "password"))
    return ()

spec :: Spec
spec = snap (route routes) app $ beforeEval fixtures $ afterEval resetDB $ do

    describe "authenticate" $ do
        it "authenticates a user and returns a token and 200 OK" $
            postJson "/domains/1/tokens" (Token.AuthParams "user1" (Pass "password")) >>= should200
        it "returns a 404 Not Found when user doesnt exist" $
            postJson "/domains/1/tokens" (Token.AuthParams "user4" (Pass "password")) >>= should404
        it "returns a 404 Not Found when password doesnt match" $
            postJson "/domains/1/tokens" (Token.AuthParams "user1" (Pass "invalid")) >>= should404

    describe "validate" $ do
        it "validates a token and returns a user record and 200 OK" $ do
            Right token <- eval $ with jwt $ JWT.issueToken "1"
            let tokenUrl = T.pack $ "/domains/1/tokens/" ++ C.unpack token
            get tokenUrl >>= (`shouldEqual` Json 200 "{\"email\":\"user1@example.com\",\"domain_id\":1,\"name\":\"user1\",\"id\":1}")
