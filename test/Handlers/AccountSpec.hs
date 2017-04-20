{-# LANGUAGE OverloadedStrings #-}

module Handlers.AccountSpec (
    spec
) where

import           Database.Persist.Class  (insert)
import           Database.Persist.Sql    (runMigrationUnsafe)
import           Snap.Core               (route)
import           Snap.Snaplet.Persistent (runPersist)
import           Test.Hspec              (Spec, describe, it)
import           Test.Hspec.Snap         (TestResponse (..), afterEval,
                                          beforeEval, delete, get, get', params,
                                          postJson, shouldEqual, snap)

import           Application             (AppHandler)
import           Helpers.DB              (resetDB)
import           Models.Account          (newAccount)
import qualified Models.Account          as Account
import           Schema                  (migrateAll)
import           Site                    (app, routes)

fixtures :: AppHandler ()
fixtures = do
    runPersist $ runMigrationUnsafe migrateAll
    _ <- runPersist . insert =<< newAccount (Account.CreateParams "account1")
    _ <- runPersist . insert =<< newAccount (Account.CreateParams "account2")
    _ <- runPersist . insert =<< newAccount (Account.CreateParams "account3")
    return ()

spec :: Spec
spec = snap (route routes) app $ beforeEval fixtures $ afterEval resetDB $ do

    describe "pagination" $ do
        it "pages threw accounts forward" $ do
            get' "/accounts" (params [("limit", "1"), ("order", "asc"), ("after", "0")]) >>= (`shouldEqual` Json 200 "[{\"name\":\"account1\",\"id\":1}]")
            get' "/accounts" (params [("limit", "1"), ("order", "asc"), ("after", "1")]) >>= (`shouldEqual` Json 200 "[{\"name\":\"account2\",\"id\":2}]")
            get' "/accounts" (params [("limit", "1"), ("order", "asc"), ("after", "2")]) >>= (`shouldEqual` Json 200 "[{\"name\":\"account3\",\"id\":3}]")
            get' "/accounts" (params [("limit", "1"), ("order", "asc"), ("after", "3")]) >>= (`shouldEqual` Json 200 "[]")
        it "pages threw accounts backwards" $ do
            get' "/accounts" (params [("limit", "1"), ("order", "desc"), ("before", "4")]) >>= (`shouldEqual` Json 200 "[{\"name\":\"account3\",\"id\":3}]")
            get' "/accounts" (params [("limit", "1"), ("order", "desc"), ("before", "3")]) >>= (`shouldEqual` Json 200 "[{\"name\":\"account2\",\"id\":2}]")
            get' "/accounts" (params [("limit", "1"), ("order", "desc"), ("before", "2")]) >>= (`shouldEqual` Json 200 "[{\"name\":\"account1\",\"id\":1}]")
            get' "/accounts" (params [("limit", "1"), ("order", "desc"), ("before", "1")]) >>= (`shouldEqual` Json 200 "[]")
        it "limits account list items" $ do
            get' "/accounts" (params [("limit", "1")]) >>= (`shouldEqual` Json 200 "[{\"name\":\"account1\",\"id\":1}]")
            get' "/accounts" (params [("limit", "2")]) >>= (`shouldEqual` Json 200 "[{\"name\":\"account1\",\"id\":1},{\"name\":\"account2\",\"id\":2}]")
            get' "/accounts" (params [("limit", "3")]) >>= (`shouldEqual` Json 200 "[{\"name\":\"account1\",\"id\":1},{\"name\":\"account2\",\"id\":2},{\"name\":\"account3\",\"id\":3}]")

    describe "listAccounts" $ do
        it "prints all the accounts in a list and returns a 200 OK" $
            get "/accounts" >>= (`shouldEqual` Json 200 "[{\"name\":\"account1\",\"id\":1},{\"name\":\"account2\",\"id\":2},{\"name\":\"account3\",\"id\":3}]")
        it "filters accounts by name" $
            get' "/accounts" (params [("name", "account1")]) >>= (`shouldEqual` Json 200 "[{\"name\":\"account1\",\"id\":1}]")

    describe "showAccount" $ do
        it "prints a account and returns a 200 OK" $
            get "/accounts/1" >>= (`shouldEqual` Json 200 "{\"name\":\"account1\",\"id\":1}")
        it "returns a 400 Bad Request when account ID is invalid" $
            get "/accounts/NaN" >>= (`shouldEqual` Other 400)
        it "returns a 404 Not Found when account is not found" $
            get "/accounts/4" >>= (`shouldEqual` NotFound)

    describe "createAccount" $
        it "creates a account and returns a 302 Found" $ do
            get "/accounts/4" >>= (`shouldEqual` NotFound)
            postJson "/accounts" (Account.CreateParams "account4") >>= (`shouldEqual` Redirect 302 "/accounts/4")
            get "/accounts/4" >>= (`shouldEqual` Json 200 "{\"name\":\"account4\",\"id\":4}")

    describe "updateAccount" $ do
        it "updates a account and returns a 204 No Content" $ do
            get "/accounts/1" >>= (`shouldEqual` Json 200 "{\"name\":\"account1\",\"id\":1}")
            postJson "/accounts/1" (Account.UpdateParams (Just "foo")) >>= (`shouldEqual` Other 204)
            get "/accounts/1" >>= (`shouldEqual` Json 200 "{\"name\":\"foo\",\"id\":1}")
        it "returns a 204 No Content when no params are submitted" $
            postJson "/accounts/1" (Account.UpdateParams Nothing) >>= (`shouldEqual` Other 204)

    describe "deleteAccount" $
        it "deletes a account and returns a 204 No Content" $ do
            get "/accounts/1" >>= (`shouldEqual` Json 200 "{\"name\":\"account1\",\"id\":1}")
            delete "/accounts/1" >>= (`shouldEqual` Other 204)
            get "/accounts/1" >>= (`shouldEqual` NotFound)
