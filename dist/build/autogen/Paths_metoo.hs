module Paths_metoo (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/gavri/.cabal/bin"
libdir     = "/home/gavri/.cabal/lib/metoo-0.0.0/ghc-7.0.3"
datadir    = "/home/gavri/.cabal/share/metoo-0.0.0"
libexecdir = "/home/gavri/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "metoo_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "metoo_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "metoo_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "metoo_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
