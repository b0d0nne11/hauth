{-# LANGUAGE OverloadedStrings #-}

module Handlers.UserSpec (
    spec
) where

import           Data.Maybe              (fromJust)
import           Database.Persist.Class  (insert)
import           Database.Persist.Sql    (runMigrationUnsafe)
import           Snap.Core               (route)
import           Snap.Snaplet.Persistent (runPersist)
import           Test.Hspec              (Spec, describe, it)
import           Test.Hspec.Snap         (TestResponse (..), afterEval,
                                          beforeEval, delete, get, get', params,
                                          postJson, shouldEqual, snap)
import           Text.Email.Validate     (emailAddress)

import           Application             (AppHandler)
import           Helpers                 (Pass (..))
import           Models.Account          (newAccount)
import qualified Models.Account          as Account
import           Models.Domain           (newDomain)
import qualified Models.Domain           as Domain
import           Models.User             (newUser)
import qualified Models.User             as User
import           Schema                  (AccountUser (..), migrateAll, resetDB)
import           Site                    (app, routes)

fixtures :: AppHandler ()
fixtures = do
    runPersist $ runMigrationUnsafe migrateAll
    d1 <- runPersist . insert $ newDomain (Domain.CreateParams "domain1")
    a1 <- runPersist . insert $ newAccount d1 (Account.CreateParams "account1")
    u1 <- runPersist . insert =<< newUser d1 (User.CreateParams "user1" (fromJust $ emailAddress "user1@example.com") (Pass "password"))
    _  <- runPersist . insert =<< newUser d1 (User.CreateParams "user2" (fromJust $ emailAddress "user2@example.com") (Pass "password"))
    _  <- runPersist . insert =<< newUser d1 (User.CreateParams "user3" (fromJust $ emailAddress "user3@example.com") (Pass "password"))
    _  <- runPersist . insert $ AccountUser a1 u1
    return ()

spec :: Spec
spec = snap (route routes) app $ beforeEval fixtures $ afterEval resetDB $ do

    describe "pagination" $ do
        it "pages threw users forward" $ do
            get' "/domains/1/users" (params [("limit", "1"), ("order", "asc"), ("after", "0")]) >>= (`shouldEqual` Json 200 "[{\"email\":\"user1@example.com\",\"domain_id\":1,\"name\":\"user1\",\"id\":1}]")
            get' "/domains/1/users" (params [("limit", "1"), ("order", "asc"), ("after", "1")]) >>= (`shouldEqual` Json 200 "[{\"email\":\"user2@example.com\",\"domain_id\":1,\"name\":\"user2\",\"id\":2}]")
            get' "/domains/1/users" (params [("limit", "1"), ("order", "asc"), ("after", "2")]) >>= (`shouldEqual` Json 200 "[{\"email\":\"user3@example.com\",\"domain_id\":1,\"name\":\"user3\",\"id\":3}]")
            get' "/domains/1/users" (params [("limit", "1"), ("order", "asc"), ("after", "3")]) >>= (`shouldEqual` Json 200 "[]")
        it "pages threw users backwards" $ do
            get' "/domains/1/users" (params [("limit", "1"), ("order", "desc"), ("before", "4")]) >>= (`shouldEqual` Json 200 "[{\"email\":\"user3@example.com\",\"domain_id\":1,\"name\":\"user3\",\"id\":3}]")
            get' "/domains/1/users" (params [("limit", "1"), ("order", "desc"), ("before", "3")]) >>= (`shouldEqual` Json 200 "[{\"email\":\"user2@example.com\",\"domain_id\":1,\"name\":\"user2\",\"id\":2}]")
            get' "/domains/1/users" (params [("limit", "1"), ("order", "desc"), ("before", "2")]) >>= (`shouldEqual` Json 200 "[{\"email\":\"user1@example.com\",\"domain_id\":1,\"name\":\"user1\",\"id\":1}]")
            get' "/domains/1/users" (params [("limit", "1"), ("order", "desc"), ("before", "1")]) >>= (`shouldEqual` Json 200 "[]")
        it "limits user list items" $ do
            get' "/domains/1/users" (params [("limit", "1")]) >>= (`shouldEqual` Json 200 "[{\"email\":\"user1@example.com\",\"domain_id\":1,\"name\":\"user1\",\"id\":1}]")
            get' "/domains/1/users" (params [("limit", "2")]) >>= (`shouldEqual` Json 200 "[{\"email\":\"user1@example.com\",\"domain_id\":1,\"name\":\"user1\",\"id\":1},{\"email\":\"user2@example.com\",\"domain_id\":1,\"name\":\"user2\",\"id\":2}]")
            get' "/domains/1/users" (params [("limit", "3")]) >>= (`shouldEqual` Json 200 "[{\"email\":\"user1@example.com\",\"domain_id\":1,\"name\":\"user1\",\"id\":1},{\"email\":\"user2@example.com\",\"domain_id\":1,\"name\":\"user2\",\"id\":2},{\"email\":\"user3@example.com\",\"domain_id\":1,\"name\":\"user3\",\"id\":3}]")

    describe "listUsers" $ do
        it "prints all the users in a list and returns a 200 OK" $
            get "/domains/1/users" >>= (`shouldEqual` Json 200 "[{\"email\":\"user1@example.com\",\"domain_id\":1,\"name\":\"user1\",\"id\":1},{\"email\":\"user2@example.com\",\"domain_id\":1,\"name\":\"user2\",\"id\":2},{\"email\":\"user3@example.com\",\"domain_id\":1,\"name\":\"user3\",\"id\":3}]")
        it "filters users by name" $
            get' "/domains/1/users" (params [("name", "user1")]) >>= (`shouldEqual` Json 200 "[{\"email\":\"user1@example.com\",\"domain_id\":1,\"name\":\"user1\",\"id\":1}]")
        it "filters users by email" $
            get' "/domains/1/users" (params [("email", "user1@example.com")]) >>= (`shouldEqual` Json 200 "[{\"email\":\"user1@example.com\",\"domain_id\":1,\"name\":\"user1\",\"id\":1}]")
        it "filters users by account" $
            get' "/domains/1/users" (params [("account_id", "1")]) >>= (`shouldEqual` Json 200 "[{\"email\":\"user1@example.com\",\"domain_id\":1,\"name\":\"user1\",\"id\":1}]")

    describe "showUser" $ do
        it "prints a user and returns a 200 OK" $
            get "/domains/1/users/1" >>= (`shouldEqual` Json 200 "{\"email\":\"user1@example.com\",\"domain_id\":1,\"name\":\"user1\",\"id\":1}")
        it "returns a 400 Bad Request when domain ID is invalid" $
            get "/domains/NaN/users/1" >>= (`shouldEqual` Other 400)
        it "returns a 400 Bad Request when user ID is invalid" $
            get "/domains/1/users/NaN" >>= (`shouldEqual` Other 400)
        it "returns a 404 Not Found when user is not found" $ do
            get "/domains/1/users/4" >>= (`shouldEqual` NotFound)
            get "/domains/4/users/1" >>= (`shouldEqual` NotFound)
            get "/domains/4/users/4" >>= (`shouldEqual` NotFound)

    describe "createUser" $ do
        it "creates a user and returns a 302 Found" $ do
            get "/domains/1/users/4" >>= (`shouldEqual` NotFound)
            postJson "/domains/1/users" (User.CreateParams "user4" (fromJust $ emailAddress "user4@example.com") (Pass "password")) >>= (`shouldEqual` Redirect 302 "/domains/1/users/4")
            get "/domains/1/users/4" >>= (`shouldEqual` Json 200 "{\"email\":\"user4@example.com\",\"domain_id\":1,\"name\":\"user4\",\"id\":4}")

    describe "updateUser" $ do
        it "updates a user and returns a 204 No Content" $ do
            get "/domains/1/users/1" >>= (`shouldEqual` Json 200 "{\"email\":\"user1@example.com\",\"domain_id\":1,\"name\":\"user1\",\"id\":1}")
            postJson "/domains/1/users/1" (User.UpdateParams (Just "foo") Nothing Nothing) >>= (`shouldEqual` Other 204)
            get "/domains/1/users/1" >>= (`shouldEqual` Json 200 "{\"email\":\"user1@example.com\",\"domain_id\":1,\"name\":\"foo\",\"id\":1}")
        it "returns a 204 No Content when no params are submitted" $
            postJson "/domains/1/users/1" (User.UpdateParams Nothing Nothing Nothing) >>= (`shouldEqual` Other 204)

    describe "deleteUser" $
        it "deletes a user and returns a 204 No Content" $ do
            get "/domains/1/users/1" >>= (`shouldEqual` Json 200 "{\"email\":\"user1@example.com\",\"domain_id\":1,\"name\":\"user1\",\"id\":1}")
            delete "/domains/1/users/1" >>= (`shouldEqual` Other 204)
            get "/domains/1/users/1" >>= (`shouldEqual` NotFound)
