{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
module Lib
    ( startApp
    , app
    ) where

import           Data.ByteString
import           Data.ByteString.Base64
import           Data.ByteString.Builder
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

import qualified Data.ByteString.Char8    as B
import qualified Data.ByteString.Lazy     as L

type Api = "decode" :> ReqBody '[PlainText] String :> Post '[PlainText] String
      :<|> "encode" :> ReqBody '[PlainText] String :> Post '[PlainText] String

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy Api
api = Proxy

server :: Server Api
server = decodeHandler :<|> encodeHandler

decodeHandler :: String -> Handler String
decodeHandler input = do
  let res = decodeBase64 input
  case res of
    Left errMsg -> throwError $ err400 { errBody = buildLazyMsg errMsg }
    Right val   -> return val

buildLazyMsg :: String -> L.ByteString
buildLazyMsg errMsg = toLazyByteString $ string8 errMsg

decodeBase64 :: String -> Either String String
decodeBase64 input = do
  let bs = B.pack input
  res <- decode bs
  return $ B.unpack res

encodeHandler :: String -> Handler String
encodeHandler input = return $ encodeBase64 input

encodeBase64 :: String -> String
encodeBase64 = B.unpack . encode . B.pack
