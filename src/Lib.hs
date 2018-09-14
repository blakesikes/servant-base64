{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
module Lib
    ( startApp
    , app
    ) where

import           Data.ByteString
import           Data.ByteString.Base64
import           Data.ByteString.Builder
import           Data.Text
import           Data.Text.Encoding
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

import qualified Data.ByteString.Char8    as B
import qualified Data.ByteString.Lazy     as L

type Api = "decode" :> ReqBody '[PlainText] Text :> Post '[PlainText] Text
      :<|> "encode" :> ReqBody '[PlainText] Text :> Post '[PlainText] Text

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy Api
api = Proxy

server :: Server Api
server = decodeHandler :<|> encodeHandler

decodeHandler :: Text-> Handler Text
decodeHandler input = do
  let res = decodeBase64 input
  case res of
    Left errMsg -> throwError $ err400 { errBody = buildLazyMsg errMsg }
    Right val   -> return val

buildLazyMsg :: String -> L.ByteString
buildLazyMsg errMsg = toLazyByteString $ string8 errMsg

decodeBase64 :: Text -> Either String Text
decodeBase64 input = do
  let inputText = encodeUtf8 input
  outputBS <- decode inputText
  return $ decodeUtf8 outputBS

encodeHandler :: Text -> Handler Text
encodeHandler input = return $ encodeBase64 input

encodeBase64 :: Text -> Text
encodeBase64 = decodeUtf8 . encode . encodeUtf8
