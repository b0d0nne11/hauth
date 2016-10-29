{-# LANGUAGE OverloadedStrings #-}

module Handlers.AccountSpec (
    spec
) where

import           Data.Maybe              (fromJust)
import           Database.Persist.Class  (insert)
import           Database.Persist.Sql    (runMigrationUnsafe, toSqlKey)
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
    _  <- runPersist . insert $ newAccount d1 (Account.CreateParams "account2")
    _  <- runPersist . insert $ newAccount d1 (Account.CreateParams "account3")
    u1 <- runPersist . insert =<< newUser d1 (User.CreateParams "user1" (fromJust $ emailAddress "user1@example.com") (Pass "password"))
    _  <- runPersist . insert =<< newUser d1 (User.CreateParams "user2" (fromJust $ emailAddress "user2@example.com") (Pass "password"))
    _  <- runPersist . insert =<< newUser d1 (User.CreateParams "user3" (fromJust $ emailAddress "user3@example.com") (Pass "password"))
    _  <- runPersist . insert $ AccountUser a1 u1
    return ()

spec :: Spec
spec = snap (route routes) app $ beforeEval fixtures $ afterEval resetDB $ do

    describe "pagination" $ do
        it "pages threw accounts forward" $ do
            get' "/domains/1/accounts" (params [("limit", "1"), ("order", "asc"), ("after", "0")]) >>= (`shouldEqual` Json 200 "[{\"domain_id\":1,\"name\":\"account1\",\"id\":1}]")
            get' "/domains/1/accounts" (params [("limit", "1"), ("order", "asc"), ("after", "1")]) >>= (`shouldEqual` Json 200 "[{\"domain_id\":1,\"name\":\"account2\",\"id\":2}]")
            get' "/domains/1/accounts" (params [("limit", "1"), ("order", "asc"), ("after", "2")]) >>= (`shouldEqual` Json 200 "[{\"domain_id\":1,\"name\":\"account3\",\"id\":3}]")
            get' "/domains/1/accounts" (params [("limit", "1"), ("order", "asc"), ("after", "3")]) >>= (`shouldEqual` Json 200 "[]")
        it "pages threw accounts backwards" $ do
            get' "/domains/1/accounts" (params [("limit", "1"), ("order", "desc"), ("before", "4")]) >>= (`shouldEqual` Json 200 "[{\"domain_id\":1,\"name\":\"account3\",\"id\":3}]")
            get' "/domains/1/accounts" (params [("limit", "1"), ("order", "desc"), ("before", "3")]) >>= (`shouldEqual` Json 200 "[{\"domain_id\":1,\"name\":\"account2\",\"id\":2}]")
            get' "/domains/1/accounts" (params [("limit", "1"), ("order", "desc"), ("before", "2")]) >>= (`shouldEqual` Json 200 "[{\"domain_id\":1,\"name\":\"account1\",\"id\":1}]")
            get' "/domains/1/accounts" (params [("limit", "1"), ("order", "desc"), ("before", "1")]) >>= (`shouldEqual` Json 200 "[]")
        it "limits account list items" $ do
            get' "/domains/1/accounts" (params [("limit", "1")]) >>= (`shouldEqual` Json 200 "[{\"domain_id\":1,\"name\":\"account1\",\"id\":1}]")
            get' "/domains/1/accounts" (params [("limit", "2")]) >>= (`shouldEqual` Json 200 "[{\"domain_id\":1,\"name\":\"account1\",\"id\":1},{\"domain_id\":1,\"name\":\"account2\",\"id\":2}]")
            get' "/domains/1/accounts" (params [("limit", "3")]) >>= (`shouldEqual` Json 200 "[{\"domain_id\":1,\"name\":\"account1\",\"id\":1},{\"domain_id\":1,\"name\":\"account2\",\"id\":2},{\"domain_id\":1,\"name\":\"account3\",\"id\":3}]")

    describe "listAccounts" $ do
        it "prints all the accounts in a list and returns a 200 OK" $
            get "/domains/1/accounts" >>= (`shouldEqual` Json 200 "[{\"domain_id\":1,\"name\":\"account1\",\"id\":1},{\"domain_id\":1,\"name\":\"account2\",\"id\":2},{\"domain_id\":1,\"name\":\"account3\",\"id\":3}]")
        it "filters accounts by name" $
            get' "/domains/1/accounts" (params [("name", "account1")]) >>= (`shouldEqual` Json 200 "[{\"domain_id\":1,\"name\":\"account1\",\"id\":1}]")
        it "filters accounts by user" $
            get' "/domains/1/accounts" (params [("user_id", "1")]) >>= (`shouldEqual` Json 200 "[{\"domain_id\":1,\"name\":\"account1\",\"id\":1}]")

    describe "showAccount" $ do
        it "prints a account and returns a 200 OK" $
            get "/domains/1/accounts/1" >>= (`shouldEqual` Json 200 "{\"domain_id\":1,\"name\":\"account1\",\"id\":1}")
        it "returns a 400 Bad Request when domain ID is invalid" $
            get "/domains/NaN/accounts/1" >>= (`shouldEqual` Other 400)
        it "returns a 400 Bad Request when account ID is invalid" $
            get "/domains/1/accounts/NaN" >>= (`shouldEqual` Other 400)
        it "returns a 404 Not Found when account is not found" $ do
            get "/domains/1/accounts/4" >>= (`shouldEqual` NotFound)
            get "/domains/4/accounts/1" >>= (`shouldEqual` NotFound)
            get "/domains/4/accounts/4" >>= (`shouldEqual` NotFound)

    describe "createAccount" $ do
        it "creates a account and returns a 302 Found" $ do
            get "/domains/1/accounts/4" >>= (`shouldEqual` NotFound)
            postJson "/domains/1/accounts" (Account.CreateParams "account4") >>= (`shouldEqual` Redirect 302 "/domains/1/accounts/4")
            get "/domains/1/accounts/4" >>= (`shouldEqual` Json 200 "{\"domain_id\":1,\"name\":\"account4\",\"id\":4}")

    describe "updateAccount" $ do
        it "updates a account and returns a 204 No Content" $ do
            get "/domains/1/accounts/1" >>= (`shouldEqual` Json 200 "{\"domain_id\":1,\"name\":\"account1\",\"id\":1}")
            postJson "/domains/1/accounts/1" (Account.UpdateParams (Just "foo") Nothing Nothing) >>= (`shouldEqual` Other 204)
            get "/domains/1/accounts/1" >>= (`shouldEqual` Json 200 "{\"domain_id\":1,\"name\":\"foo\",\"id\":1}")
        it "returns a 204 No Content when no params are submitted" $
            postJson "/domains/1/accounts/1" (Account.UpdateParams Nothing Nothing Nothing) >>= (`shouldEqual` Other 204)
        it "links a user to an account and return a 204 No Content" $ do
            get' "/domains/1/accounts" (params [("user_id", "2")]) >>= (`shouldEqual` Json 200 "[]")
            postJson "/domains/1/accounts/2" (Account.UpdateParams Nothing (Just [toSqlKey 2]) Nothing) >>= (`shouldEqual` Other 204)
            get' "/domains/1/accounts" (params [("user_id", "2")]) >>= (`shouldEqual` Json 200 "[{\"domain_id\":1,\"name\":\"account2\",\"id\":2}]")
        it "unlinks a user to an account and return a 204 No Content" $ do
            get' "/domains/1/accounts" (params [("user_id", "1")]) >>= (`shouldEqual` Json 200 "[{\"domain_id\":1,\"name\":\"account1\",\"id\":1}]")
            postJson "/domains/1/accounts/1" (Account.UpdateParams Nothing Nothing (Just [toSqlKey 1])) >>= (`shouldEqual` Other 204)
            get' "/domains/1/accounts" (params [("user_id", "1")]) >>= (`shouldEqual` Json 200 "[]")
        it "linking and unlinking users is idempotent" $ do
            postJson "/domains/1/accounts/1" (Account.UpdateParams Nothing (Just [toSqlKey 1]) Nothing) >>= (`shouldEqual` Other 204)
            postJson "/domains/1/accounts/1" (Account.UpdateParams Nothing (Just [toSqlKey 1]) Nothing) >>= (`shouldEqual` Other 204)
            postJson "/domains/1/accounts/1" (Account.UpdateParams Nothing Nothing (Just [toSqlKey 1])) >>= (`shouldEqual` Other 204)
            postJson "/domains/1/accounts/1" (Account.UpdateParams Nothing Nothing (Just [toSqlKey 1])) >>= (`shouldEqual` Other 204)

    describe "deleteAccount" $
        it "deletes a account and returns a 204 No Content" $ do
            get "/domains/1/accounts/1" >>= (`shouldEqual` Json 200 "{\"domain_id\":1,\"name\":\"account1\",\"id\":1}")
            delete "/domains/1/accounts/1" >>= (`shouldEqual` Other 204)
            get "/domains/1/accounts/1" >>= (`shouldEqual` NotFound)
