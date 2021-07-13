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
version = Version [0,9,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/bumrap/Documents/code/MaximumCity/.stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/0dd4bee0f15b10eafa9e2b7b3f08565f5ea16dbf2fa219444e6b70ba4afe0355/8.6.5/bin"
libdir     = "/Users/bumrap/Documents/code/MaximumCity/.stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/0dd4bee0f15b10eafa9e2b7b3f08565f5ea16dbf2fa219444e6b70ba4afe0355/8.6.5/lib/x86_64-linux-ghc-8.6.5/MaximumCity-0.9.1-5B1Cv7JRrszHKNYX1c7Fy9"
dynlibdir  = "/Users/bumrap/Documents/code/MaximumCity/.stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/0dd4bee0f15b10eafa9e2b7b3f08565f5ea16dbf2fa219444e6b70ba4afe0355/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/Users/bumrap/Documents/code/MaximumCity/.stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/0dd4bee0f15b10eafa9e2b7b3f08565f5ea16dbf2fa219444e6b70ba4afe0355/8.6.5/share/x86_64-linux-ghc-8.6.5/MaximumCity-0.9.1"
libexecdir = "/Users/bumrap/Documents/code/MaximumCity/.stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/0dd4bee0f15b10eafa9e2b7b3f08565f5ea16dbf2fa219444e6b70ba4afe0355/8.6.5/libexec/x86_64-linux-ghc-8.6.5/MaximumCity-0.9.1"
sysconfdir = "/Users/bumrap/Documents/code/MaximumCity/.stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/0dd4bee0f15b10eafa9e2b7b3f08565f5ea16dbf2fa219444e6b70ba4afe0355/8.6.5/etc"

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
