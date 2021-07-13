{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Aws.Lambda
import Aws.Lambda.Runtime.Common (LambdaError (..))
import Lib
import Telegram

main :: IO ()
main = do
  -- tc <- getTelegramSettings
  res <- getTelegramSettings
  case res of 
    Left msg -> ((pure
                 . (Left
                      . (ApiGatewayLambdaError
                           . (mkApiGatewayResponse 500 . toApiGatewayResponseBody))))
                $ ("Telegram Settings "
                     <> (msg <> " is not set")))
    Right tc -> runLambda (run tc)
  where
    run :: TC -> LambdaOptions -> IO (Either String LambdaResult)
    run tc opts = do
      result <- handler tc (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
      either (pure . Left . encodeObj) (pure . Right . LambdaResult . encodeObj) result