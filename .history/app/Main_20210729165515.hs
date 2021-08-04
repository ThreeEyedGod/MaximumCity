{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Aws.Lambda
import Lib
import Telegram
--import Debug.Trace (trace)

main :: IO ()
main = do
  --res <- getTelegramSettings
  --case res of
    --Left msg -> runLambda (runLH msg undefined) -- if we could not get Telegram Token
    --Right tc -> runLambda (runLH "" tc)
    thisCall :: IO context
    runLambda thisCall runCB
  where
    runCB :: APIGatewayHandlerType (LambdaOptions context) -> IO (Either LambdaError LambdaResult)
    runCB opts = do
        result <- handleTelegram (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
        either (pure . Left . LambdaError . encodeObj) (pure . Right . LambdaResult . encodeObj) result

    {--runLH :: String -> TC -> (LambdaOptions context) -> IO (Either LambdaError LambdaResult)
    runLH s tc opts
      | not (Prelude.null s) = do -- no telegram token. Route it to bad handler
          result <- badhandler s (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
          either (pure . Left . LambdaError . encodeObj) (pure . Right . LambdaResult . encodeObj) result
      | otherwise = do -- OK case regular handler will deal with it
          result <- handler tc (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
          either (pure . Left . LambdaError . encodeObj) (pure . Right . LambdaResult . encodeObj) result
    --