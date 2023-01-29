{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_appy (
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
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/xuanlang/study/hskell/exam/code/appy/.stack-work/install/x86_64-osx/cd16c01118d9a9516027bae7f0beaa03c397239ca8983c7f91a44e2952921102/9.0.2/bin"
libdir     = "/Users/xuanlang/study/hskell/exam/code/appy/.stack-work/install/x86_64-osx/cd16c01118d9a9516027bae7f0beaa03c397239ca8983c7f91a44e2952921102/9.0.2/lib/x86_64-osx-ghc-9.0.2/appy-0.0.0-901ut1wmmuSECw6fh0m8zh-primary-test-suite"
dynlibdir  = "/Users/xuanlang/study/hskell/exam/code/appy/.stack-work/install/x86_64-osx/cd16c01118d9a9516027bae7f0beaa03c397239ca8983c7f91a44e2952921102/9.0.2/lib/x86_64-osx-ghc-9.0.2"
datadir    = "/Users/xuanlang/study/hskell/exam/code/appy/.stack-work/install/x86_64-osx/cd16c01118d9a9516027bae7f0beaa03c397239ca8983c7f91a44e2952921102/9.0.2/share/x86_64-osx-ghc-9.0.2/appy-0.0.0"
libexecdir = "/Users/xuanlang/study/hskell/exam/code/appy/.stack-work/install/x86_64-osx/cd16c01118d9a9516027bae7f0beaa03c397239ca8983c7f91a44e2952921102/9.0.2/libexec/x86_64-osx-ghc-9.0.2/appy-0.0.0"
sysconfdir = "/Users/xuanlang/study/hskell/exam/code/appy/.stack-work/install/x86_64-osx/cd16c01118d9a9516027bae7f0beaa03c397239ca8983c7f91a44e2952921102/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "appy_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "appy_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "appy_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "appy_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "appy_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "appy_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
