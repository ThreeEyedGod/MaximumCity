{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
                    

module InterfaceAdapters.Utils.ParsingLHJSON where

import GHC.Generics (Generic)
import Data.Aeson (eitherDecode)
import Data.Aeson.Types (FromJSON)
import Text.RawString.QQ
import Data.ByteString (ByteString)
import Data.Either (isRight, partitionEithers)
import Data.ByteString.Lazy.UTF8 (fromString) -- from utf8-string
-----------------------------------------------------------
-- Liquid Haskell imports
import Data.Text as T
import Data.Text.Unsafe as UT

-- Specs: http://ucsd-progsys.github.io/liquidhaskell/specifications/
{-@ measure txtLen :: Text -> Int @-}

{-@ assume T.pack :: i:String -> {o:T.Text | len i == txtLen o } @-}
-----------------------------------------------------------

jsonValid   = [r| { "locations": ["Europe", "US", "Asia"], "payload": "Important" } |] :: String
jsonInvalid = [r| { "locations": ["Europe"], "payload": "Important" } |] :: String

jsonInvalidX = [r| { "locations": [], "payload": "Important" } |] :: String
jsonInvalidXX = [r| { "locations": ["",""], "payload": "Important" } |] :: String
jsonInvalidY = [r| { "locations": [], "payload": "" } |] :: String
jsonInvalidZ = [r| { } |] :: String

-- | Raw data that will firstly be parsed by Aeson and then predicate-parsed by LH helpers
data RawData = RawData
    { locations :: [String]
    , payload   :: String
    } deriving (Show, Eq, Ord, Generic, FromJSON)

-- Domain Data
{-@ type TextNE = {v:T.Text | 0 < txtLen v} @-}
{-@ type Destinations = {ls: [ v:TextNE ] | 2 <= len ls}  @-}
type Destinations = [T.Text]

-- | redundant replication of important data, at least 2 locations should be provided.
--   Note that it's safe to unpack 'Destinations' because LH verifies that the list
--   contains at least two items.
{-@ apiCallProvideRedundancy :: ls : Destinations -> d : TextNE -> (Bool, T.Text)  @-}
apiCallProvideRedundancy :: Destinations -> Text -> (Bool, T.Text)
apiCallProvideRedundancy (first: second: rest) dat =
    (True, "Data was persisted in at least '" <> first <> "' and '" <> second <> "' locations")


test :: [IO ()]
--test = mapM_ (processAPI . eitherDecode) [jsonValid, jsonInvalid]
test = Prelude.map (processAPI . eitherDecode . fromString) [jsonValid]


processAPI :: Either String RawData -> IO ()
processAPI json = do
    case json of
        Left invalid -> print invalid
        Right (RawData locs dat) ->
            case (parsedLocs, parsedData) of
                (Right (l : ll : lx), Right d) ->
                    print $ apiCallProvideRedundancy (l : ll : lx) d
                _ -> print "Input data has incorrect shape"
            where
                parsedLocs = case locs of
                    []           -> Left "invalid destinations value-null"
                    [x]          -> Left "Invalid! Only 1 element. Destinations needs min of 2! "
                    (x : xx: xs) -> Right . filterInvalid $ locs
                    _            -> Left "invalid destinations value-other"

                parsedData = case dat of
                    (x:_)   -> Right . T.pack $ dat
                    _       -> Left "Invalid-null or other"

{-@  filterInvalid :: x:[String] -> rv:[TextNE] @-}
filterInvalid :: [String] -> [Text]
filterInvalid = snd . partitionEithers . Prelude.map nonEmptyData 

{-@ nonEmptyData :: x:String  -> rv : (Either String {rght:TextNE | txtLen rght == len x})   @-}
nonEmptyData :: String -> Either String Text
nonEmptyData x = case x of
    [] -> Left x
    "" -> Left x
    _  -> Right $ T.pack x