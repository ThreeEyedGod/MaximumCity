{-@ LIQUID "--higherorder" @-}
{-@ LIQUID "--save" @-}
{-@ LIQUID "--prune-unsorted" @-}
{-@ LIQUID "--typed-holes" @-}
{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple"            @-}
--{-@ LIQUID "--compilespec" @-}
{-@ LIQUID "--no-class-check" @-}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--no-totality" @-}
{-@ LIQUID "--short-names" @-}
module InterfaceAdapters.Weather.Tutorial where
import GHC.Types
import Data.List.NonEmpty (head, NonEmpty, nonEmpty)
import System.Environment (getEnv)
import Control.Exception (throwIO)
import GHC.Utils.Misc (split)

{-@ embed Int * as Int @-}
{-@ type NonZero = { z: Int | z /= 0} @-}
{-@ type Pos = { z: Int | z > 0} @-}

-- | A *divide* function that *only accepts* non-zero denominators
{-@ divide :: Int -> NonZero -> Int @-}
divide :: Int -> Int -> Int 
divide _ 0 = error "divide-by-zero"
divide x n = x `div` n

notEmpty :: [a] -> Bool
notEmpty []    = False
notEmpty (_:_) = True
{-@ measure notEmpty :: [a] -> Bool @-}

{-@ type NEList a = {v:[a] | notEmpty v} @-}

{-@ measure sz :: {xs :[a] | len xs > 0 } -> {v: Nat | v /= 0 }  @-}
sz :: [a] -> Int
sz []           = 0
sz (x : xs) = 1 + sz xs
{-@ lazy sz @-}

{-@ impossible :: { s: String | False } -> a @-}
impossible :: String -> a 
impossible message = error message 

{-@ average :: {x: [Int] | len x > 0 } -> Int @-}
-- the elems +1 works but 1 + elems does not ! 
average :: [Int] -> Int
average (x : xs) = divide total (elems + 1)
  where
    total  = x + sum xs
    elems  = length xs
average _        = impossible "not possible"

getConfigurationDirectories :: IO (NonEmpty FilePath)
getConfigurationDirectories = do
  configDirsString <- getEnv "PATH"
  let configDirsList = split ',' configDirsString
  case nonEmpty configDirsList of
    Just nonEmptyConfigDirsList -> pure nonEmptyConfigDirsList
    Nothing -> throwIO $ userError "CONFIG_DIRS cannot be empty"
 
 {-@ predicate Btwn Lo V Hi = (Lo <= V && V <= Hi) @-}
test :: IO FilePath
test = do
 configDirs <- getConfigurationDirectories
 pure (Data.List.NonEmpty.head configDirs)

{-@ test1 :: {y : Int | y > 0 } -> IO Int @-}
test1 :: Int -> IO Int
test1 n = pure $ a * 1
  where
   a0 = 0:[0,1..n]
   a = average a0


{-@ size :: {x: [Int] | len x > 0 } -> {y: Pos | y > 0 && y == len x && y /= 0  }  @-}
size        :: [Int] -> Int
size nel@(_ : xs)  =  1 + length xs
size []            =  impossible "can't reach here"

{-@ predicate Lt X Y = X < Y        @-}
{-@ predicate Ge X Y = not (Lt X Y) @-}
{-@ predicate Lte X Y = X <= Y        @-}
{-@ predicate Gt X Y = not (Lte X Y) @-}
{-@ predicate Ne X Y = X /= Y @-}
