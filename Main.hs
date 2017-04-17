{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import qualified Data.Map.Strict (assocs, lookup)
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Version
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Simple
import Options.Applicative
import System.Directory
import System.Exit
import System.FilePath
import System.Process (rawSystem)

import Stackage.Types hiding (unPackageName)
import Data.Foldable (traverse_)
import Data.Yaml hiding (Parser)
import Distribution.Package (PackageName(..), unPackageName)

data Args = Args Command

newtype UrlOption = UrlOption {showurl :: Bool}

newtype ConsumerOption = ConsumerOption {threshold :: Int}

data Command = List UrlOption SnapshotType [String]
             | Config UrlOption SnapshotType
             | Ghc SnapshotType
             | Core SnapshotType
             | Tools SnapshotType
             | Packages SnapshotType
             | Consumers ConsumerOption SnapshotType
             | Package SnapshotType String
             | Users SnapshotType String
             | Github SnapshotType String

urlParser :: Parser UrlOption
urlParser = UrlOption <$> switch
  (long "url" <> short 'u' <> help "Show url")

consumeParser :: Parser ConsumerOption
consumeParser = ConsumerOption <$> option auto
  (long "minimum" <> short 'm' <> metavar "THRESHOLD" <> value 5 <> help "Show packages with more than MINIMUM consumers (default 5)")

parseString :: String -> Parser String
parseString lbl = strArgument $ metavar lbl

parseSnap :: Parser SnapshotType
parseSnap = argument snap $ metavar "SNAP"
  where
    snap :: ReadM SnapshotType
    snap = maybeReader readSnap

    readSnap :: String -> Maybe SnapshotType
    readSnap "nightly" = Just STNightly
    readSnap "lts" = Just $ STLTS 8 9
    readSnap _ = Nothing

commandParser :: Parser Command
commandParser =
  subparser $
  command "list" (info (List <$> urlParser <*> parseSnap <*> some (parseString "PKG..."))
                   (progDesc "Show Stackage SNAP version of PKGs"))
  <>
  command "config" (info (Config <$> urlParser <*> parseSnap)
                    (progDesc "Download cabal.config file for SNAP"))
  <>
  command "ghc" (info (Ghc <$> parseSnap)
                  (progDesc "GHC version for SNAP"))
  <>
  command "core" (info (Core <$> parseSnap)
                   (progDesc "Core packages for SNAP"))
  <>
  command "tools" (info (Tools <$> parseSnap)
                    (progDesc "Tools for SNAP"))
  <>
  command "packages" (info (Packages <$> parseSnap)
                       (progDesc "All packages in SNAP"))
  <>
  command "consumers" (info (Consumers <$> consumeParser <*> parseSnap)
                       (progDesc "No of users of packages in SNAP"))
  <>
  command "package" (info (Package <$> parseSnap <*> parseString "PKG")
                      (progDesc "PKG info for SNAP"))
  <>
  command "users" (info (Users <$> parseSnap <*> parseString "PKG")
                    (progDesc "Revdeps for PKG in SNAP"))
  <>
  command "github" (info (Github <$> parseSnap <*> parseString "PKG")
                     (progDesc "Owners for PKG in SNAP"))

argsParser :: Parser Args
argsParser = Args <$> commandParser

main :: IO ()
main = do
  cmd <- customExecParser (prefs showHelpOnEmpty)
         (info (helper <*> argsParser) $ progDesc "Stackage query tool")
  run cmd
  where
    run (Args cmd) =
      case cmd of
        List opts s ps -> stackageList opts s ps
        Config opts s -> stackageConfig opts s
        Ghc s -> buildplanGHC s
        Core s -> buildplanCore s
        Tools s -> buildplanTools s
        Packages s -> buildplanPackages s
        Consumers opts s -> buildplanConsumers opts s
        Package s pkg -> buildplanPackage s pkg
        Users s pkg -> buildplanUsers s pkg
        Github s pkg -> buildplanGithub s pkg

topurl :: String
topurl = "https://www.stackage.org/"

stackageList :: UrlOption -> SnapshotType -> [String] -> IO ()
stackageList opts s ps = do
  mgr <- newManager tlsManagerSettings
  mapM_ (sendReq mgr) ps
  where
    sendReq mgr p = do
      let pkgurl = topurl ++ showSnap s </> "package/"
      req <- parseRequest $ pkgurl ++ p
      hist <- responseOpenHistory req mgr
      let redirs = mapMaybe (lookup "Location" . responseHeaders . snd) $ hrRedirects hist
      if null redirs
        then giveup $ show req
        else do
        let loc = B.unpack $ last redirs
        if topurl `isPrefixOf` loc
          then putStrLn (if showurl opts then loc else takeFileName loc)
          else giveup p

    giveup u = die $ u ++ " not found in " ++ showSnap s


stackageConfig :: UrlOption -> SnapshotType -> IO ()
stackageConfig opts snap = do
  let url = topurl ++ showSnap snap </> "cabal.config"
  req <- parseRequest url
  response <- httpLBS req
  L.writeFile "cabal.config" $ getResponseBody response
  when (showurl opts) $
    putStrLn url

getBuildPlan :: SnapshotType -> IO BuildPlan
getBuildPlan snap = do
  home <- getHomeDirectory
  let configDir = home </> ".stackage-query"
  haveDir <- doesDirectoryExist configDir
  unless haveDir $ createDirectory configDir
  let project = snapProject snap
      projectDir = configDir </> show project
  haveProj <- doesDirectoryExist projectDir
  unless haveProj $ cloneProject configDir project
  let yaml = projectDir </> showSnap snap ++ ".yaml"
  haveYaml <- doesFileExist yaml
  when (not haveYaml) $ updateProject projectDir
  ebp <- decodeFileEither yaml
  either (error . prettyPrintParseException) return ebp

data Project = LTS | Nightly

instance Show Project where
  show LTS = "lts-haskell"
  show Nightly = "stackage-nightly"

snapProject :: SnapshotType -> Project
snapProject (STLTS _ _) = LTS
snapProject _ = Nightly

showSnap :: SnapshotType -> String
showSnap STNightly = "nightly-2017-04-12"
showSnap (STNightly2 date) = "nightly-" ++ show date
showSnap (STLTS major minor) = "lts-" ++ show major ++ "." ++ show minor

cmd_ :: String -> [String] -> IO ()
cmd_ c args = do
  ret <- rawSystem c args
  case ret of
    ExitSuccess -> return ()
    ExitFailure n -> error $ "\"" ++ unwords (c:args) ++ "\" failed with exit code " ++ show n

cloneProject :: FilePath -> Project -> IO ()
cloneProject dir proj = do
  let url = "git://github.com/fpco" </> show proj
  putStrLn $ "Cloning " ++ url
  cmd_ "git" ["-C", dir, "clone", url]

updateProject :: FilePath -> IO ()
updateProject dir = do
  cmd_ "git" ["-C", dir, "pull"]

buildplanGHC :: SnapshotType -> IO ()
buildplanGHC snap = do
  bp <- getBuildPlan snap
  let si = bpSystemInfo bp
  putStrLn $ (showVersion . siGhcVersion) si

buildplanCore :: SnapshotType -> IO ()
buildplanCore snap = do
  bp <- getBuildPlan snap
  let si = bpSystemInfo bp
  traverse_ (putStrLn . showPkgVer) $ (Data.Map.Strict.assocs . siCorePackages) si

buildplanTools :: SnapshotType -> IO ()
buildplanTools snap = do
  bp <- getBuildPlan snap
  traverse_ (putStrLn . showPkgVer) $ bpTools bp

buildplanPackages :: SnapshotType -> IO ()
buildplanPackages snap = do
  bp <- getBuildPlan snap
  traverse_ (putStrLn . showPkgVer) $ Data.Map.Strict.assocs $ fmap ppVersion $ bpPackages bp

buildplanConsumers :: ConsumerOption -> SnapshotType -> IO ()
buildplanConsumers opts snap = do
  bp <- getBuildPlan snap
  traverse_ putPkgConsumption $ Data.Map.Strict.assocs $ fmap ppUsers $ bpPackages bp
  where
    putPkgConsumption :: (PackageName, Set.Set PackageName) -> IO ()
    putPkgConsumption (p, users) = do
      let n = length users
      when (n >= threshold opts) $
        putStrLn $ (show n) ++ " " ++ unPackageName p

evalPackageBuildPlan :: SnapshotType -> String -> (PackagePlan -> String) -> IO ()
evalPackageBuildPlan snap pkg expr = do
  bp <- getBuildPlan snap
  let mpkgplan = Data.Map.Strict.lookup (PackageName pkg) $ bpPackages bp
  putStrLn $ maybe "Package not found" expr mpkgplan

buildplanPackage :: SnapshotType -> String -> IO ()
buildplanPackage snap pkg =
  evalPackageBuildPlan snap pkg (showVersion . ppVersion)

buildplanUsers :: SnapshotType -> String -> IO ()
buildplanUsers snap pkg =
  evalPackageBuildPlan snap pkg (unwords . Set.elems . Set.map unPackageName . ppUsers)

buildplanGithub :: SnapshotType -> String -> IO ()
buildplanGithub snap pkg =
  evalPackageBuildPlan snap pkg (unwords . Set.elems . Set.map T.unpack . ppGithubPings)

showPkgVer :: (PackageName, Version) -> String
showPkgVer (p,v) = unPackageName p ++ " " ++ showVersion v
