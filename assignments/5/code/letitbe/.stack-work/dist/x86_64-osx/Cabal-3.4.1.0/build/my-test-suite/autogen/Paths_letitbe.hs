{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_letitbe (
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

bindir     = "/Users/xuanlang/study/hskell/assignments/5/code/letitbe/.stack-work/install/x86_64-osx/cd16c01118d9a9516027bae7f0beaa03c397239ca8983c7f91a44e2952921102/9.0.2/bin"
libdir     = "/Users/xuanlang/study/hskell/assignments/5/code/letitbe/.stack-work/install/x86_64-osx/cd16c01118d9a9516027bae7f0beaa03c397239ca8983c7f91a44e2952921102/9.0.2/lib/x86_64-osx-ghc-9.0.2/letitbe-0.0.0-80dEUdAC40N5YvR6V6Vgdw-my-test-suite"
dynlibdir  = "/Users/xuanlang/study/hskell/assignments/5/code/letitbe/.stack-work/install/x86_64-osx/cd16c01118d9a9516027bae7f0beaa03c397239ca8983c7f91a44e2952921102/9.0.2/lib/x86_64-osx-ghc-9.0.2"
datadir    = "/Users/xuanlang/study/hskell/assignments/5/code/letitbe/.stack-work/install/x86_64-osx/cd16c01118d9a9516027bae7f0beaa03c397239ca8983c7f91a44e2952921102/9.0.2/share/x86_64-osx-ghc-9.0.2/letitbe-0.0.0"
libexecdir = "/Users/xuanlang/study/hskell/assignments/5/code/letitbe/.stack-work/install/x86_64-osx/cd16c01118d9a9516027bae7f0beaa03c397239ca8983c7f91a44e2952921102/9.0.2/libexec/x86_64-osx-ghc-9.0.2/letitbe-0.0.0"
sysconfdir = "/Users/xuanlang/study/hskell/assignments/5/code/letitbe/.stack-work/install/x86_64-osx/cd16c01118d9a9516027bae7f0beaa03c397239ca8983c7f91a44e2952921102/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "letitbe_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "letitbe_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "letitbe_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "letitbe_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "letitbe_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "letitbe_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
