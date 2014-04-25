-- | haste-inst - Haste wrapper for cabal.
module Main where
import System.FilePath
import System.Environment
import System.Directory
import System.IO
import Haste.Environment
import Control.Monad

type Match = (String -> Bool, [String] -> [String])

config :: String
config = unlines
  [ "-- This is the (haste) configuration file for the 'cabal' command line tool."
  , " remote-repo: hackage.haskell.org:http://hackage.haskell.org/packages/archive"
  , " remote-repo-cache: " ++ pkgDir
  , " with-compiler: " ++ hasteBinary
  , " with-hc-pkg: " ++ hastePkgBinary
  , " package-db: " ++ pkgDir
  , ""
  , "install-dirs user"
  , "  prefix: " ++ hasteInstDir
  , ""
  , "program-locations"
  , "  hsc2hs-location: hsc2hs"
  , ""
  ]

configFile :: FilePath
configFile = hasteDir </> "config"

cabal :: [String] -> IO ()
cabal args = do
  putStrLn $ unwords $ "cabal":allArgs
  runAndWait "cabal" allArgs Nothing
  where
    configArg = "--config-file=" ++ (hasteDir </> "config")
    allArgs = hasteargs ++ args
    hasteargs
      | "build" `elem` args  = ["--with-ghc=" ++ hasteBinary]
      | "update" `elem` args = [configArg]
      | otherwise            = [configArg,"-fhaste-inst"]

main :: IO ()
main = do
  -- Add a config file if we need to.
  configExists <- doesFileExist configFile
  unless configExists $ do
      putStrLn $ "Writing haste-inst config file to " ++ configFile
      h <- openFile configFile WriteMode
      hPutStr h config
      hClose h
      putStrLn $ "Downloading the package list."
      cabal ["update"]

  as  <- getArgs
  as' <- return $ if not ("update" `elem` as) && ("--install-jsmods" `elem` as || not ("build" `elem` as))
                    then libinstall : filter (/= "--install-jsmods") as
                    else as
  as'' <- return $ if "--unbooted" `elem` as'
                     then unbooted : filter (/= "--unbooted") as'
                     else as
  cabal as''
  where
    libinstall = "--ghc-option=--libinstall"
    unbooted   = "--ghc-option=--unbooted"
