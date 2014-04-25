{-# LANGUAGE CPP #-}
-- | haste-pkg; wrapper for ghc-pkg.
module Main where
import Control.Monad
import System.Environment
import System.Directory
import Haste.Environment

main :: IO ()
main = do
  args <- getArgs
  pkgDirExists <- doesDirectoryExist pkgDir
  when (not pkgDirExists) $ do
    putStrLn "Creating package directory."
    createDirectoryIfMissing True pkgLibDir
    runAndWait "ghc-pkg" ["init", pkgDir] Nothing
  putStrLn $ unwords $ "ghc-pkg":ghcPkgArgs args
  runAndWait "ghc-pkg" (ghcPkgArgs args) Nothing
  where
    ghcPkgArgs args = packages ++ map userToGlobal args
#if __GLASGOW_HASKELL__ >= 706
    packages = ["--no-user-package-db",
                "--global-package-db=" ++ pkgDir]
#else
    packages = ["--no-user-package-conf",
                "--global-conf=" ++ pkgDir]
#endif
    userToGlobal "--user" = "--global"
    userToGlobal str      = str

