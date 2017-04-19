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
import           Helpers.Crypto          (Pass (..))
import           Helpers.DB              (resetDB)
import           Models.Account          (newAccount)
import qualified Models.Account          as Account
import           Models.User             (newUser)
import qualified Models.User             as User
import           Schema                  (migrateAll)
import           Site                    (app, routes)

fixtures :: AppHandler ()
fixtures = do
    runPersist $ runMigrationUnsafe migrateAll
    a1 <- runPersist . insert =<< newAccount (Account.CreateParams "account1")
    _  <- runPersist . insert =<< newUser a1 (User.CreateParams "user1" (fromJust $ emailAddress "user1@example.com") (Pass "password"))
    _  <- runPersist . insert =<< newUser a1 (User.CreateParams "user2" (fromJust $ emailAddress "user2@example.com") (Pass "password"))
    _  <- runPersist . insert =<< newUser a1 (User.CreateParams "user3" (fromJust $ emailAddress "user3@example.com") (Pass "password"))
    return ()

spec :: Spec
spec = snap (route routes) app $ beforeEval fixtures $ afterEval resetDB $ do

    describe "pagination" $ do
        it "pages threw users forward" $ do
            get' "/accounts/1/users" (params [("limit", "1"), ("order", "asc"), ("after", "0")]) >>= (`shouldEqual` Json 200 "[{\"email\":\"user1@example.com\",\"name\":\"user1\",\"id\":1,\"account_id\":1}]")
            get' "/accounts/1/users" (params [("limit", "1"), ("order", "asc"), ("after", "1")]) >>= (`shouldEqual` Json 200 "[{\"email\":\"user2@example.com\",\"name\":\"user2\",\"id\":2,\"account_id\":1}]")
            get' "/accounts/1/users" (params [("limit", "1"), ("order", "asc"), ("after", "2")]) >>= (`shouldEqual` Json 200 "[{\"email\":\"user3@example.com\",\"name\":\"user3\",\"id\":3,\"account_id\":1}]")
            get' "/accounts/1/users" (params [("limit", "1"), ("order", "asc"), ("after", "3")]) >>= (`shouldEqual` Json 200 "[]")
        it "pages threw users backwards" $ do
            get' "/accounts/1/users" (params [("limit", "1"), ("order", "desc"), ("before", "4")]) >>= (`shouldEqual` Json 200 "[{\"email\":\"user3@example.com\",\"name\":\"user3\",\"id\":3,\"account_id\":1}]")
            get' "/accounts/1/users" (params [("limit", "1"), ("order", "desc"), ("before", "3")]) >>= (`shouldEqual` Json 200 "[{\"email\":\"user2@example.com\",\"name\":\"user2\",\"id\":2,\"account_id\":1}]")
            get' "/accounts/1/users" (params [("limit", "1"), ("order", "desc"), ("before", "2")]) >>= (`shouldEqual` Json 200 "[{\"email\":\"user1@example.com\",\"name\":\"user1\",\"id\":1,\"account_id\":1}]")
            get' "/accounts/1/users" (params [("limit", "1"), ("order", "desc"), ("before", "1")]) >>= (`shouldEqual` Json 200 "[]")
        it "limits user list items" $ do
            get' "/accounts/1/users" (params [("limit", "1")]) >>= (`shouldEqual` Json 200 "[{\"email\":\"user1@example.com\",\"name\":\"user1\",\"id\":1,\"account_id\":1}]")
            get' "/accounts/1/users" (params [("limit", "2")]) >>= (`shouldEqual` Json 200 "[{\"email\":\"user1@example.com\",\"name\":\"user1\",\"id\":1,\"account_id\":1},{\"email\":\"user2@example.com\",\"name\":\"user2\",\"id\":2,\"account_id\":1}]")
            get' "/accounts/1/users" (params [("limit", "3")]) >>= (`shouldEqual` Json 200 "[{\"email\":\"user1@example.com\",\"name\":\"user1\",\"id\":1,\"account_id\":1},{\"email\":\"user2@example.com\",\"name\":\"user2\",\"id\":2,\"account_id\":1},{\"email\":\"user3@example.com\",\"name\":\"user3\",\"id\":3,\"account_id\":1}]")

    describe "listUsers" $ do
        it "prints all the users in a list and returns a 200 OK" $
            get "/accounts/1/users" >>= (`shouldEqual` Json 200 "[{\"email\":\"user1@example.com\",\"name\":\"user1\",\"id\":1,\"account_id\":1},{\"email\":\"user2@example.com\",\"name\":\"user2\",\"id\":2,\"account_id\":1},{\"email\":\"user3@example.com\",\"name\":\"user3\",\"id\":3,\"account_id\":1}]")
        it "filters users by name" $
            get' "/accounts/1/users" (params [("name", "user1")]) >>= (`shouldEqual` Json 200 "[{\"email\":\"user1@example.com\",\"name\":\"user1\",\"id\":1,\"account_id\":1}]")
        it "filters users by email" $
            get' "/accounts/1/users" (params [("email", "user1@example.com")]) >>= (`shouldEqual` Json 200 "[{\"email\":\"user1@example.com\",\"name\":\"user1\",\"id\":1,\"account_id\":1}]")

    describe "showUser" $ do
        it "prints a user and returns a 200 OK" $
            get "/accounts/1/users/1" >>= (`shouldEqual` Json 200 "{\"email\":\"user1@example.com\",\"name\":\"user1\",\"id\":1,\"account_id\":1}")
        it "returns a 400 Bad Request when account ID is invalid" $
            get "/accounts/NaN/users/1" >>= (`shouldEqual` Other 400)
        it "returns a 400 Bad Request when user ID is invalid" $
            get "/accounts/1/users/NaN" >>= (`shouldEqual` Other 400)
        it "returns a 404 Not Found when user is not found" $ do
            get "/accounts/1/users/4" >>= (`shouldEqual` NotFound)
            get "/accounts/4/users/1" >>= (`shouldEqual` NotFound)
            get "/accounts/4/users/4" >>= (`shouldEqual` NotFound)

    describe "createUser" $
        it "creates a user and returns a 302 Found" $ do
            get "/accounts/1/users/4" >>= (`shouldEqual` NotFound)
            postJson "/accounts/1/users" (User.CreateParams "user4" (fromJust $ emailAddress "user4@example.com") (Pass "password")) >>= (`shouldEqual` Redirect 302 "/accounts/1/users/4")
            get "/accounts/1/users/4" >>= (`shouldEqual` Json 200 "{\"email\":\"user4@example.com\",\"name\":\"user4\",\"id\":4,\"account_id\":1}")

    describe "updateUser" $ do
        it "updates a user and returns a 204 No Content" $ do
            get "/accounts/1/users/1" >>= (`shouldEqual` Json 200 "{\"email\":\"user1@example.com\",\"name\":\"user1\",\"id\":1,\"account_id\":1}")
            postJson "/accounts/1/users/1" (User.UpdateParams (Just "foo") Nothing Nothing) >>= (`shouldEqual` Other 204)
            get "/accounts/1/users/1" >>= (`shouldEqual` Json 200 "{\"email\":\"user1@example.com\",\"name\":\"foo\",\"id\":1,\"account_id\":1}")
        it "returns a 204 No Content when no params are submitted" $
            postJson "/accounts/1/users/1" (User.UpdateParams Nothing Nothing Nothing) >>= (`shouldEqual` Other 204)

    describe "deleteUser" $
        it "deletes a user and returns a 204 No Content" $ do
            get "/accounts/1/users/1" >>= (`shouldEqual` Json 200 "{\"email\":\"user1@example.com\",\"name\":\"user1\",\"id\":1,\"account_id\":1}")
            delete "/accounts/1/users/1" >>= (`shouldEqual` Other 204)
            get "/accounts/1/users/1" >>= (`shouldEqual` NotFound)
