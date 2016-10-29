{-# LANGUAGE OverloadedStrings #-}

module Handlers.DomainSpec (
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
import           Models.Domain           (newDomain)
import qualified Models.Domain           as Domain
import           Schema                  (migrateAll, resetDB)
import           Site                    (app, routes)

fixtures :: AppHandler ()
fixtures = do
    runPersist $ runMigrationUnsafe migrateAll
    _ <- runPersist . insert $ newDomain (Domain.CreateParams "domain1")
    _ <- runPersist . insert $ newDomain (Domain.CreateParams "domain2")
    _ <- runPersist . insert $ newDomain (Domain.CreateParams "domain3")
    return ()

spec :: Spec
spec = snap (route routes) app $ beforeEval fixtures $ afterEval resetDB $ do

    describe "pagination" $ do
        it "pages threw domains forward" $ do
            get' "/domains" (params [("limit", "1"), ("order", "asc"), ("after", "0")]) >>= (`shouldEqual` Json 200 "[{\"name\":\"domain1\",\"id\":1}]")
            get' "/domains" (params [("limit", "1"), ("order", "asc"), ("after", "1")]) >>= (`shouldEqual` Json 200 "[{\"name\":\"domain2\",\"id\":2}]")
            get' "/domains" (params [("limit", "1"), ("order", "asc"), ("after", "2")]) >>= (`shouldEqual` Json 200 "[{\"name\":\"domain3\",\"id\":3}]")
            get' "/domains" (params [("limit", "1"), ("order", "asc"), ("after", "3")]) >>= (`shouldEqual` Json 200 "[]")
        it "pages threw domains backwards" $ do
            get' "/domains" (params [("limit", "1"), ("order", "desc"), ("before", "4")]) >>= (`shouldEqual` Json 200 "[{\"name\":\"domain3\",\"id\":3}]")
            get' "/domains" (params [("limit", "1"), ("order", "desc"), ("before", "3")]) >>= (`shouldEqual` Json 200 "[{\"name\":\"domain2\",\"id\":2}]")
            get' "/domains" (params [("limit", "1"), ("order", "desc"), ("before", "2")]) >>= (`shouldEqual` Json 200 "[{\"name\":\"domain1\",\"id\":1}]")
            get' "/domains" (params [("limit", "1"), ("order", "desc"), ("before", "1")]) >>= (`shouldEqual` Json 200 "[]")
        it "limits domain list items" $ do
            get' "/domains" (params [("limit", "1")]) >>= (`shouldEqual` Json 200 "[{\"name\":\"domain1\",\"id\":1}]")
            get' "/domains" (params [("limit", "2")]) >>= (`shouldEqual` Json 200 "[{\"name\":\"domain1\",\"id\":1},{\"name\":\"domain2\",\"id\":2}]")
            get' "/domains" (params [("limit", "3")]) >>= (`shouldEqual` Json 200 "[{\"name\":\"domain1\",\"id\":1},{\"name\":\"domain2\",\"id\":2},{\"name\":\"domain3\",\"id\":3}]")

    describe "listDomains" $ do
        it "prints all the domains in a list and returns a 200 OK" $
            get "/domains" >>= (`shouldEqual` Json 200 "[{\"name\":\"domain1\",\"id\":1},{\"name\":\"domain2\",\"id\":2},{\"name\":\"domain3\",\"id\":3}]")
        it "filters domains by name" $
            get' "/domains" (params [("name", "domain1")]) >>= (`shouldEqual` Json 200 "[{\"name\":\"domain1\",\"id\":1}]")

    describe "showDomain" $ do
        it "prints a domain and returns a 200 OK" $
            get "/domains/1" >>= (`shouldEqual` Json 200 "{\"name\":\"domain1\",\"id\":1}")
        it "returns a 400 Bad Request when domain ID is invalid" $
            get "/domains/NaN" >>= (`shouldEqual` Other 400)
        it "returns a 404 Not Found when domain is not found" $
            get "/domains/4" >>= (`shouldEqual` NotFound)

    describe "createDomain" $ do
        it "creates a domain and returns a 302 Found" $ do
            get "/domains/4" >>= (`shouldEqual` NotFound)
            postJson "/domains" (Domain.CreateParams "domain4") >>= (`shouldEqual` Redirect 302 "/domains/4")
            get "/domains/4" >>= (`shouldEqual` Json 200 "{\"name\":\"domain4\",\"id\":4}")

    describe "updateDomain" $ do
        it "updates a domain and returns a 204 No Content" $ do
            get "/domains/1" >>= (`shouldEqual` Json 200 "{\"name\":\"domain1\",\"id\":1}")
            postJson "/domains/1" (Domain.UpdateParams $ Just "foo") >>= (`shouldEqual` Other 204)
            get "/domains/1" >>= (`shouldEqual` Json 200 "{\"name\":\"foo\",\"id\":1}")
        it "returns a 204 No Content when no params are submitted" $
            postJson "/domains/1" (Domain.UpdateParams Nothing) >>= (`shouldEqual` Other 204)

    describe "deleteDomain" $
        it "deletes a domain and returns a 204 No Content" $ do
            get "/domains/1" >>= (`shouldEqual` Json 200 "{\"name\":\"domain1\",\"id\":1}")
            delete "/domains/1" >>= (`shouldEqual` Other 204)
            get "/domains/1" >>= (`shouldEqual` NotFound)
