{-@ LIQUID "--higherorder" @-}
{-@ LIQUID "--save" @-}
{-@ LIQUID "--prune-unsorted" @-}
{-@ LIQUID "--typed-holes" @-}
{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple"            @-}
--{-@ LIQUID "--compilespec" @-}
{-@ LIQUID "--no-class-check" @-}
--{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--no-totality" @-}
{-@ LIQUID "--short-names" @-}
module InterfaceAdapters.Weather.Tutorial where
import GHC.Types
import Data.List.NonEmpty
import System.Environment (getEnv)
import Control.Exception (throwIO)
import GHC.Utils.Misc (split)

{-@ embed Int * as Int @-}
{-@ one :: {v:Int | v == 1 } @-}
one :: Int
one = 1


{-@ type Even = {v:Int | v mod 2 = 0} @-}


{-@ notEven :: Even @-}
notEven = 8

{-@ isEven :: n:Nat -> {v:Bool | (v <=> (n mod 2 == 0))} @-}   
isEven   :: Int -> Bool 
isEven 0 = True
isEven 1 = False
isEven n = not (isEven (n-1))
{-@ type NonZero = {v:Int | v /= 0} @-}
{-@ type Pos = {v:Int | v > 0} @-}

notEven    :: Int
-- | A *divide* function that *only accepts* non-zero denominators
{-@ divide :: Int -> NonZero -> Int @-}
divide :: Int -> Int -> Int 
divide _ 0 = error "divide-by-zero"
divide x n = x `div` n

avg2 :: Int -> Int -> Int
avg2 x y   = divide (x + y) 2

{-@ size :: xs:[a] -> v:Nat @-}
--{-@ size :: xs:[a] -> Nat @-}
size        :: [a] -> Int
size []     =  0 
size (_:xs) =  1 + size xs

notEmpty :: [a] -> Bool
notEmpty []    = False
notEmpty (_:_) = True
{-@ measure notEmpty :: [a] -> Bool @-}

{-@ measure len :: forall a. [a] -> GHC.Types.Int @-}
len :: [a] -> GHC.Types.Int
len ([])     = 0
len (y:ys)   = 1 + len(ys)


{-@ type NEList a = {v:[a] | notEmpty v} @-}
{-@ avgMany :: NEList Int -> Int @-}
avgMany :: [Int] -> Int
avgMany xs = divide total elems
  where
    total  = sum  xs
    elems  = size xs

getConfigurationDirectories :: IO (NonEmpty FilePath)
getConfigurationDirectories = do
  configDirsString <- getEnv "CONFIG_DIRS"
  let configDirsList = split ',' configDirsString
  case nonEmpty configDirsList of
    Just nonEmptyConfigDirsList -> pure nonEmptyConfigDirsList
    Nothing -> throwIO $ userError "CONFIG_DIRS cannot be empty"

test :: IO FilePath
test = do
 configDirs <- getConfigurationDirectories
 pure (Data.List.NonEmpty.head configDirs)