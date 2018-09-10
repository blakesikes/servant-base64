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
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

import qualified Data.ByteString.Char8    as B
import qualified Data.ByteString.Lazy     as L

instance MimeUnrender PlainText B.ByteString where
  mimeUnrender _ = Right . L.toStrict

instance MimeRender PlainText B.ByteString where
  mimeRender _ = L.fromStrict

type Api = "decode" :> ReqBody '[PlainText] B.ByteString :> Post '[PlainText] B.ByteString
      :<|> "encode" :> ReqBody '[PlainText] B.ByteString :> Post '[PlainText] B.ByteString

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy Api
api = Proxy

server :: Server Api
server = decodeHandler :<|> encodeHandler

decodeHandler :: B.ByteString -> Handler B.ByteString
decodeHandler input = do
  let res = decodeBase64 input
  case res of
    Left errMsg -> throwError $ err400 { errBody = buildLazyMsg errMsg }
    Right val   -> return val

buildLazyMsg :: String -> L.ByteString
buildLazyMsg errMsg = toLazyByteString $ string8 errMsg

decodeBase64 :: B.ByteString -> Either String B.ByteString
decodeBase64 = decode

encodeHandler :: B.ByteString -> Handler B.ByteString
encodeHandler input = return $ encode input
