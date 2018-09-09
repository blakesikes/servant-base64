{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Lib                 (app)
import           Network.HTTP.Types
import           Network.Wai.Test    (SResponse (simpleBody, simpleStatus))
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
    describe "POST /decode" $ do
        it "responds with 200" $ do
            r <- request methodPost "/decode" [("Content-Type", "text/plain;charset=utf-8")] "VGVzdGluZy4uLg=="
            liftIO $ simpleStatus r `shouldBe` status200
        it "responds with Testing..." $ do
            r <- request methodPost "/decode" [("Content-Type", "text/plain;charset=utf-8")] "VGVzdGluZy4uLg=="
            liftIO $ simpleBody r `shouldBe` "Testing..."
        it "respond with 400" $ do
            r <- request methodPost "/decode" [("Content-Type", "text/plain;charset=utf-8")] "gibberish"
            liftIO $ simpleStatus r `shouldBe` status400
    describe "POST /encode" $ do
        it "responds with 200" $ do
            r <- request methodPost "/encode" [("Content-Type", "text/plain;charset=utf-8")] "Testing..."
            liftIO $ simpleStatus r `shouldBe` status200
        it "responds encoded" $ do
            r <- request methodPost "/encode" [("Content-Type", "text/plain;charset=utf-8")] "Testing..."
            liftIO $ simpleBody r `shouldBe` "VGVzdGluZy4uLg=="
