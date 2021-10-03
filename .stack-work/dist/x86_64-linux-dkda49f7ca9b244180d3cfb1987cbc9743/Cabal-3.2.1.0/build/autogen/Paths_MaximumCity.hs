{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_MaximumCity (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,9,9] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/bumrap/Documents/Code/MaximumCity/.stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/d8229e83210b5bb5fb0c1da9da3467710effccb1b3f2966d0e4169c88c410d06/8.10.5/bin"
libdir     = "/Users/bumrap/Documents/Code/MaximumCity/.stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/d8229e83210b5bb5fb0c1da9da3467710effccb1b3f2966d0e4169c88c410d06/8.10.5/lib/x86_64-linux-ghc-8.10.5/MaximumCity-0.9.9-3qUfKD51vkgHyM6aIft8Pq"
dynlibdir  = "/Users/bumrap/Documents/Code/MaximumCity/.stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/d8229e83210b5bb5fb0c1da9da3467710effccb1b3f2966d0e4169c88c410d06/8.10.5/lib/x86_64-linux-ghc-8.10.5"
datadir    = "/Users/bumrap/Documents/Code/MaximumCity/.stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/d8229e83210b5bb5fb0c1da9da3467710effccb1b3f2966d0e4169c88c410d06/8.10.5/share/x86_64-linux-ghc-8.10.5/MaximumCity-0.9.9"
libexecdir = "/Users/bumrap/Documents/Code/MaximumCity/.stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/d8229e83210b5bb5fb0c1da9da3467710effccb1b3f2966d0e4169c88c410d06/8.10.5/libexec/x86_64-linux-ghc-8.10.5/MaximumCity-0.9.9"
sysconfdir = "/Users/bumrap/Documents/Code/MaximumCity/.stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/d8229e83210b5bb5fb0c1da9da3467710effccb1b3f2966d0e4169c88c410d06/8.10.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "MaximumCity_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "MaximumCity_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "MaximumCity_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "MaximumCity_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "MaximumCity_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "MaximumCity_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
