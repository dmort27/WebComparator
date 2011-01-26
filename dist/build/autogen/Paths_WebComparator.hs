module Paths_WebComparator (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/mortensen/.cabal/bin"
libdir     = "/Users/mortensen/.cabal/lib/WebComparator-0.1/ghc-6.12.3"
datadir    = "/Users/mortensen/.cabal/share/WebComparator-0.1"
libexecdir = "/Users/mortensen/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "WebComparator_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "WebComparator_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "WebComparator_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "WebComparator_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
