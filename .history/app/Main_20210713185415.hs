{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Aws.Lambda
import Lib
import Telegram
import Debug.Trace (trace)

main :: IO ()
main = do
  -- tc <- getTelegramSettings
  res <- getTelegramSettings
  case res of 
    Left msg -> trace ("Left Main = " ++ show msg) $ runLambda (runError msg)
    Right tc -> trace ("Right Main = " ++ "tc") $ runLambda (run tc)
  where
    runError :: String -> LambdaOptions -> IO (Either String LambdaResult)
    runError s opts = do
      result <- badhandler s (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
      Right tc -> trace ("Right Main = " ++ "tc") $ runLambda (run tc)either (pure . Left . encodeObj) (pure . Right . LambdaResult . encodeObj) result
    run :: TC -> LambdaOptions -> IO (Either String LambdaResult)
    run tc opts = do
      result <- handler tc (decodeObj (eventObject opts)) (decodeObj (contextObject opts))
      either (pure . Left . encodeObj) (pure . Right . LambdaResult . encodeObj) result