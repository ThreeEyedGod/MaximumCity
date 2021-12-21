{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

module ExternalInterfaces.ServantShim
  ( makeHandler
  ) where

import           Protolude

import           AWSLambda.Events.APIGateway
import           Data.Aeson                  (Value, decode, encode)
import           Data.Aeson.Embedded         (Embedded)
import           Data.ByteString.Builder     (toLazyByteString)
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Lazy        as BL
import           Data.IORef                  (IORef, newIORef, readIORef,
                                              writeIORef)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           GHC.IO.Exception            (IOError, IOErrorType (OtherError),
                                              IOException (IOError))
import           Network.AWS.Lens            ((%~), (?~), (^.))
import           Network.HTTP.Types          hiding (Header)
import           Network.Wai                 (Application,
                                              RequestBodyLength (ChunkedBody),
                                              defaultRequest)
import           Network.Wai.Internal        (Request (..), Response (..),
                                              ResponseReceived (..))


type APIGatewayHandler
   = APIGatewayProxyRequest (Embedded Value) -> IO (APIGatewayProxyResponse (Embedded Value))

-- | Constructs a APIGatewayHandler for a request to this AWS Lambda function.
--
-- A handler converts the APIGatewayProxyRequest into a regular WAI Request
-- value. This is then passed into the Servant application and handled as if it
-- was a regular Servant request.
--
-- The passed Application is used to handle all requests coming into the handler.
makeHandler :: Application -> APIGatewayHandler
makeHandler application apiGatewayRequest = do
  responseRef <- newIORef Protolude.undefined
  request <- flip proxyToRequest apiGatewayRequest <$> newIORef reqBody
  _ <- application request $ streamResponseTo responseRef
  result <- readIORef responseRef
  pure . responseFromBody . decode $ result
  where
    reqBody =
      maybe mempty (BL.toStrict . encode) $
      apiGatewayRequest ^. requestBodyEmbedded

-- | Streams an HTTP response into an IORef, confirming receipt of the response.
--
-- This is intended to be used to construct the second argument to the WAI
-- Application type, a (Response -> IO ResponseReceived) function. This allows
-- us to extract the response from a WAI application without sending that
-- response to the client.
streamResponseTo :: IORef BL.ByteString -> Response -> IO ResponseReceived
streamResponseTo responseRef resp = do
  body <-
    case resp of
      ResponseBuilder _ _ builder -> pure $ toLazyByteString builder
      _ ->
        throwIO $
        IOError
          Nothing
          OtherError
          ""
          "unable to process response"
          Nothing
          Nothing
  writeIORef responseRef body
  pure ResponseReceived

addCorsHeaders :: ResponseHeaders -> ResponseHeaders
addCorsHeaders headers = allowOrigin : allowCredentials : headers
  where
    allowOrigin = ("Access-Control-Allow-Origin", "*")
    allowCredentials = ("Access-Control-Allow-Credentials", "true")

responseFromBody :: Maybe Value -> APIGatewayProxyResponse (Embedded Value)
--responseFromBody Nothing = responseBadRequest -- telegram repeatedly sends request if it was sent this
responseFromBody Nothing = responseOK
responseFromBody (Just body) =
  responseOK & responseBodyEmbedded ?~ body & agprsHeaders %~ addCorsHeaders

proxyToRequest ::
     IORef B.ByteString -> APIGatewayProxyRequest (Embedded Value) -> Request
proxyToRequest bodyRef apiGWRequest =
  defaultRequest
    { requestMethod = apiGWRequest ^. agprqHttpMethod
    , httpVersion = http11
    , rawPathInfo = rawPath
    , rawQueryString = query
    , requestHeaders = apiGWRequest ^. agprqHeaders
    , pathInfo = pathParts
    , queryString = apiGWRequest ^. agprqQueryStringParameters
    , Network.Wai.Internal.requestBody = ioBody
    , requestBodyLength = ChunkedBody
    }
  where
    rawPath = apiGWRequest ^. agprqPath
    pathParts = drop 1 . T.splitOn "/" $ T.decodeUtf8 rawPath
    query = renderQuery True $ apiGWRequest ^. agprqQueryStringParameters
    ioBody = do
      b <- readIORef bodyRef
      writeIORef bodyRef B.empty
      pure b