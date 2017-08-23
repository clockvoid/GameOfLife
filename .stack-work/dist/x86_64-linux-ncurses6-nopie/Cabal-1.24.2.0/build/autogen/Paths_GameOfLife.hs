{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_GameOfLife (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/clock/haskell/GameOfLife/.stack-work/install/x86_64-linux-ncurses6-nopie/lts-9.1/8.0.2/bin"
libdir     = "/home/clock/haskell/GameOfLife/.stack-work/install/x86_64-linux-ncurses6-nopie/lts-9.1/8.0.2/lib/x86_64-linux-ghc-8.0.2/GameOfLife-0.1.0.0-6o3f9I2XcP646eMLv4Syr2"
dynlibdir  = "/home/clock/haskell/GameOfLife/.stack-work/install/x86_64-linux-ncurses6-nopie/lts-9.1/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/clock/haskell/GameOfLife/.stack-work/install/x86_64-linux-ncurses6-nopie/lts-9.1/8.0.2/share/x86_64-linux-ghc-8.0.2/GameOfLife-0.1.0.0"
libexecdir = "/home/clock/haskell/GameOfLife/.stack-work/install/x86_64-linux-ncurses6-nopie/lts-9.1/8.0.2/libexec"
sysconfdir = "/home/clock/haskell/GameOfLife/.stack-work/install/x86_64-linux-ncurses6-nopie/lts-9.1/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "GameOfLife_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "GameOfLife_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "GameOfLife_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "GameOfLife_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "GameOfLife_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "GameOfLife_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
