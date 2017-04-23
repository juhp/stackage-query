{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Char (isDigit)
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
import System.Process (rawSystem, readProcess)
import Text.ParserCombinators.ReadP (readP_to_S)

import Stackage.Types hiding (unPackageName)
import Data.Foldable (traverse_)
import Data.Yaml hiding (Parser)
import Distribution.Package (PackageName(..), unPackageName)

newtype Args = Args Command

newtype UrlOption = UrlOption {showurl :: Bool}

newtype ConsumerOption = ConsumerOption {threshold :: Int}

data Snapshot = LatestNightly | LatestLTS | NightlySnap String | LtsSnap String | LtsMajor String

instance Show Snapshot where
  show LatestNightly = "nightly"
  show LatestLTS = "lts"
  show (NightlySnap date) = "nightly-" ++ date
  show (LtsSnap ver) = "lts-" ++ ver
  show (LtsMajor ver) = "lts-" ++ ver

instance Read Snapshot where
  readsPrec _ "nightly" = [(LatestNightly,"")]
  readsPrec _ "lts" = [(LatestLTS,"")]
  readsPrec _ input | "nightly-" `isPrefixOf` input =
    let (date,rest) = span (\ c -> isDigit c || c == '-') $ removePrefix "nightly-" input in
       [(NightlySnap date,rest)]
  readsPrec _ input | "lts-" `isPrefixOf` input =
    let (ver,rest) = span (\ c -> isDigit c || c == '.') $ removePrefix "lts-" input in
      [(if '.' `elem` ver then LtsSnap ver else LtsMajor ver,rest)]
  readsPrec _ _ = []

removePrefix :: String -> String-> String
removePrefix pref orig = fromMaybe orig (stripPrefix pref orig)

data Project = LTS | Nightly deriving (Eq)

instance Show Project where
  show LTS = "lts-haskell"
  show Nightly = "stackage-nightly"

instance Read Project where
  readsPrec _ "lts" = [(LTS,"")]
  readsPrec _ "nightly" = [(Nightly,"")]
  readsPrec _ _ = []

data Command = List UrlOption Snapshot [String]
             | Config UrlOption Snapshot
             | Ghc Snapshot
             | Core Snapshot
             | Tools Snapshot
             | Packages Snapshot
             | Consumers ConsumerOption Snapshot
             | Package Snapshot String
             | Users Snapshot String
             | Github Snapshot String
             | Latest Project
             | Update Project

urlParser :: Parser UrlOption
urlParser = UrlOption <$> switch
  (long "url" <> short 'u' <> help "Show url")

consumeParser :: Parser ConsumerOption
consumeParser = ConsumerOption <$> option auto
  (long "minimum" <> short 'm' <> metavar "THRESHOLD" <> value 5 <> help "Show packages with at least THRESHOLD consumers (default 5)")

parseString :: String -> Parser String
parseString lbl = strArgument $ metavar lbl

parseSnap :: Parser Snapshot
parseSnap = argument auto $ metavar "SNAP"

parseProject :: Parser Project
parseProject = argument auto $ metavar "PROJECT"

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
  <>
  command "latest" (info (Latest <$> parseProject)
                     (progDesc "Latest snap for PROJECT (nightly or lts)"))
  <>
  command "update" (info (Update <$> parseProject)
                     (progDesc "git update PROJECT (nightly or lts)"))

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
        Latest prj -> buildplanLatest prj
        Update prj -> buildplanUpdate prj

topurl :: String
topurl = "https://www.stackage.org/"

stackageList :: UrlOption -> Snapshot -> [String] -> IO ()
stackageList opts s ps = do
  mgr <- newManager tlsManagerSettings
  mapM_ (sendReq mgr) ps
  where
    sendReq mgr p = do
      let pkgurl = topurl ++ show s </> "package/"
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

    giveup u = die $ u ++ " not found in " ++ show s


stackageConfig :: UrlOption -> Snapshot -> IO ()
stackageConfig opts snap = do
  let url = topurl ++ show snap </> "cabal.config"
  req <- parseRequest url
  response <- httpLBS req
  L.writeFile "cabal.config" $ getResponseBody response
  when (showurl opts) $
    putStrLn url

getConfigDir :: IO FilePath
getConfigDir = do
  home <- getHomeDirectory
  let confdir = home </> ".stackage-query"
  createDirectoryIfMissing False confdir
  return confdir

getProjectDir :: Project -> IO FilePath
getProjectDir project = do
  configDir <- getConfigDir
  let projectDir = configDir </> show project
  haveProj <- doesDirectoryExist projectDir
  unless haveProj $ cloneProject configDir project
  return projectDir

findBuildPlanYaml :: Snapshot -> IO FilePath
findBuildPlanYaml snap = do
  projectDir <- getProjectDir $ snapProject snap
  findSnap True projectDir snap

getBuildPlan :: Snapshot -> IO BuildPlan
getBuildPlan snap = do
  ebp <- findBuildPlanYaml snap >>= decodeFileEither
  either (error . prettyPrintParseException) return ebp

snapProject :: Snapshot -> Project
snapProject LatestLTS = LTS
snapProject (LtsSnap _) = LTS
snapProject (LtsMajor _) = LTS
snapProject _ = Nightly

findSnap :: Bool -> FilePath -> Snapshot -> IO FilePath
findSnap update dir snap = do
  fs <-  sortProject . filter (show snap `isPrefixOf`) <$> getDirectoryContents dir
  if null fs
    then
    if update
      then do
      updateProject dir
      findSnap False dir snap
      else
      error $ "Snap " ++ show snap ++ " not found"
    else return (dir </> last fs ++ ".yaml")
  where
    sortProject :: [String] -> [String]
    sortProject =
      if snapProject snap == Nightly
      then sort
      else map (("lts-" ++) . showVersion) . sort . map (readVersion . takeBaseName. removePrefix "lts-")

    readVersion :: String -> Version
    readVersion = fst . last . readP_to_S parseVersion

system :: String -> [String] -> IO String
system c args = removeTrailingNewline <$> readProcess c args ""
  where
    removeTrailingNewline :: String -> String
    removeTrailingNewline "" = ""
    removeTrailingNewline cs =
      if last cs == '\n'
      then init cs
      else cs

system_ :: String -> [String] -> IO ()
system_ c args = do
  ret <- rawSystem c args
  case ret of
    ExitSuccess -> return ()
    ExitFailure n -> error $ "\"" ++ unwords (c:args) ++ "\" failed with exit code " ++ show n

git :: FilePath -> String -> [String] -> IO String
git dir cmd args =
  system "git" $ ["-C", dir] ++ cmd:args

git_ :: FilePath -> String -> [String] -> IO ()
git_ dir cmd args =
  system_ "git" $ ["-C", dir] ++ cmd:args

cloneProject :: FilePath -> Project -> IO ()
cloneProject dir proj = do
  let url = "git://github.com/fpco" </> show proj
  putStrLn $ "Cloning " ++ url
  git_ dir "clone" [url]

updateProject :: FilePath -> IO ()
updateProject dir = do
  msg <- shortLog
  _ <- git dir "pull" []
  msg' <- shortLog
  when (msg /= msg') $ putStrLn $ msg ++ " -> " ++ msg'
  where
    shortLog =
      removePrefix "Checking in " . unwords . tail . words <$> git dir "log" ["--oneline", "-1"]

buildplanGHC :: Snapshot -> IO ()
buildplanGHC snap = do
  bp <- getBuildPlan snap
  let si = bpSystemInfo bp
  putStrLn $ (showVersion . siGhcVersion) si

showPkgVer :: (PackageName, Version) -> String
showPkgVer (p,v) = unPackageName p ++ " " ++ showVersion v

buildplanCore :: Snapshot -> IO ()
buildplanCore snap = do
  bp <- getBuildPlan snap
  let si = bpSystemInfo bp
  traverse_ (putStrLn . showPkgVer) $ (Data.Map.Strict.assocs . siCorePackages) si

buildplanTools :: Snapshot -> IO ()
buildplanTools snap = do
  bp <- getBuildPlan snap
  traverse_ (putStrLn . showPkgVer) $ bpTools bp

buildplanPackages :: Snapshot -> IO ()
buildplanPackages snap = do
  bp <- getBuildPlan snap
  traverse_ (putStrLn . showPkgVer) $ Data.Map.Strict.assocs $ ppVersion <$> bpPackages bp

buildplanConsumers :: ConsumerOption -> Snapshot -> IO ()
buildplanConsumers opts snap = do
  bp <- getBuildPlan snap
  traverse_ putPkgConsumption $ Data.Map.Strict.assocs $ ppUsers <$> bpPackages bp
  where
    putPkgConsumption :: (PackageName, Set.Set PackageName) -> IO ()
    putPkgConsumption (p, users) = do
      let n = length users
      when (n >= threshold opts) $
        putStrLn $ show n ++ " " ++ unPackageName p

evalPackageBuildPlan :: Snapshot -> String -> (PackagePlan -> String) -> IO ()
evalPackageBuildPlan snap pkg expr = do
  bp <- getBuildPlan snap
  let mpkgplan = Data.Map.Strict.lookup (PackageName pkg) $ bpPackages bp
  putStrLn $ maybe "Package not found" expr mpkgplan

buildplanPackage :: Snapshot -> String -> IO ()
buildplanPackage snap pkg =
  evalPackageBuildPlan snap pkg (showVersion . ppVersion)

buildplanUsers :: Snapshot -> String -> IO ()
buildplanUsers snap pkg =
  evalPackageBuildPlan snap pkg (unwords . Set.elems . Set.map unPackageName . ppUsers)

buildplanGithub :: Snapshot -> String -> IO ()
buildplanGithub snap pkg =
  evalPackageBuildPlan snap pkg (unwords . Set.elems . Set.map T.unpack . ppGithubPings)

projectToSnap :: Project -> Snapshot
projectToSnap Nightly = LatestNightly
projectToSnap LTS = LatestLTS

buildplanLatest :: Project -> IO ()
buildplanLatest prj = do
  latest <- takeBaseName <$> findBuildPlanYaml (projectToSnap prj)
  putStrLn latest

buildplanUpdate :: Project -> IO ()
buildplanUpdate project =
  getProjectDir project >>= updateProject
