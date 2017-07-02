{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Char (isDigit)
import Data.List
import qualified Data.Map.Strict (assocs, keys, lookup)
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Version
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

import Paths_stackage_query (version)

newtype Args = Args Command

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

type Pkg = String

data Command = Config Snapshot
             | Ghc Snapshot
             | Core Snapshot
             | Tools Snapshot (Maybe Pkg)
             | Packages Snapshot
             | Consumers ConsumerOption Snapshot
             | Package Snapshot Pkg
             | Users Snapshot Pkg
             | Github Snapshot Pkg
             | Constraints Snapshot Pkg
             | Dependencies Snapshot Pkg
             | Executables Snapshot Pkg
             | Modules Snapshot Pkg
             | Latest Project
             | Update Project

consumeParser :: Parser ConsumerOption
consumeParser = ConsumerOption <$> option auto
  (long "minimum" <> short 'm' <> metavar "THRESHOLD" <> value 5 <> help "Show packages with at least THRESHOLD consumers (default 5)")

parsePkg :: Pkg -> Parser Pkg
parsePkg lbl = strArgument $ metavar lbl

parseSnap :: Parser Snapshot
parseSnap = argument auto $ metavar "SNAP"

parseProject :: Parser Project
parseProject = argument auto $ metavar "PROJECT"

commandParser :: Parser Command
commandParser =
  subparser $
  command "package" (info (Package <$> parseSnap <*> parsePkg "PKG")
                      (progDesc "Show version of PKG in SNAP"))
  <>
  command "users" (info (Users <$> parseSnap <*> (parsePkg "PKG"))
                    (progDesc "Revdeps for PKG in SNAP"))
  <>
  command "github" (info (Github <$> parseSnap <*> parsePkg "PKG")
                     (progDesc "Stackage owners for PKG in SNAP"))
  <>
  command "constraints" (info (Constraints <$> parseSnap <*> parsePkg "PKG")
                         (progDesc "Stackage constraints for PKG in SNAP"))
  <>
  command "dependencies" (info (Dependencies <$> parseSnap <*> parsePkg "PKG")
                          (progDesc "Dependencies for PKG in SNAP"))
  <>
  command "executables" (info (Executables <$> parseSnap <*> parsePkg "PKG")
                          (progDesc "Executables of PKG in SNAP"))
  <>
  command "modules" (info (Modules <$> parseSnap <*> parsePkg "PKG")
                          (progDesc "Modules of PKG in SNAP"))
  <>
  command "latest" (info (Latest <$> parseProject)
                     (progDesc "Latest snap for PROJECT (nightly or lts)"))
  <>
  command "update" (info (Update <$> parseProject)
                     (progDesc "git update PROJECT (nightly or lts)"))
  <>
  command "ghc" (info (Ghc <$> parseSnap)
                  (progDesc "GHC version for SNAP"))
  <>
  command "core" (info (Core <$> parseSnap)
                   (progDesc "GHC core libraries for SNAP"))
  <>
  command "tools" (info (Tools <$> parseSnap <*> optional (parsePkg "PKG"))
                    (progDesc "Tools for SNAP"))
  <>
  command "packages" (info (Packages <$> parseSnap)
                       (progDesc "All packages in SNAP"))
  <>
  command "consumers" (info (Consumers <$> consumeParser <*> parseSnap)
                       (progDesc "No of users of packages in SNAP"))
  <>
  command "config" (info (Config <$> parseSnap)
                    (progDesc "Download cabal.config file for SNAP"))

argsParser :: Parser Args
argsParser = Args <$> commandParser

main :: IO ()
main = do
  cmd <- customExecParser (prefs showHelpOnEmpty)
         (info (helper <*> versionOption <*> argsParser) $ progDesc "Stackage query tool")
  run cmd
  where
    run (Args cmd) =
      case cmd of
        Config s -> stackageConfig s
        Ghc s -> buildplanGHC s
        Core s -> buildplanCore s
        Tools s mpkg -> buildplanTools s mpkg
        Packages s -> buildplanPackages s
        Consumers opts s -> buildplanConsumers opts s
        Package s pkg -> buildplanPackage s pkg
        Users s pkgs -> buildplanUsers s pkgs
        Github s pkg -> buildplanGithub s pkg
        Constraints s pkg -> buildplanConstraints s pkg
        Dependencies s pkg -> buildplanDependencies s pkg
        Executables s pkg -> buildplanExecutables s pkg
        Modules s pkg -> buildplanModules s pkg
        Latest prj -> buildplanLatest prj
        Update prj -> buildplanUpdate prj

    versionOption =
        infoOption
            (showVersion version)
            (long "version" <> help "Show version")

topurl :: String
topurl = "https://www.stackage.org/"

stackageConfig :: Snapshot -> IO ()
stackageConfig snap = do
  let url = topurl ++ show snap </> "cabal.config"
  system_ "curl" ["-L", "-O", url]

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

findBuildPlanYaml :: Bool -> Snapshot -> IO FilePath
findBuildPlanYaml update snap = do
  dir <- getProjectDir $ snapProject snap
  when update $ updateProject False dir
  findSnap True dir snap

getBuildPlan :: Snapshot -> IO BuildPlan
getBuildPlan snap = do
  ebp <- findBuildPlanYaml False snap >>= decodeFileEither
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
      updateProject False dir
      findSnap False dir snap
      else
      error $ "Snap " ++ show snap ++ " not found"
    else return (dir </> last fs)
  where
    sortProject :: [String] -> [String]
    sortProject =
      if snapProject snap == Nightly
      then sort
      else map ((\v -> "lts-" ++ v ++ ".yaml") . showVersion) . sort . map (readVersion . takeBaseName . removePrefix "lts-")

readVersion :: String -> Version
readVersion s =
  let vp = readP_to_S parseVersion s in
    if null vp then giveup
    else case last vp of
           (ver,"") -> ver
           _ -> giveup
  where
    giveup = error $ "readVersion: failed to parse " ++ s

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

updateProject :: Bool -> FilePath -> IO ()
updateProject verbose dir = do
  msg <- shortLog
  _ <- git dir "pull" ["-q"]
  msg' <- shortLog
  when (verbose && msg /= msg') $ putStrLn $ msg ++ " -> " ++ msg'
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

buildplanTools :: Snapshot -> Maybe Pkg -> IO ()
buildplanTools snap mpkg = do
  bp <- getBuildPlan snap
  maybe (traverse_ (putStrLn . showPkgVer) $ bpTools bp) (buildplanPkgTools snap) mpkg

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

evalPackageBuildPlan :: Snapshot -> Pkg -> (PackagePlan -> String) -> IO ()
evalPackageBuildPlan snap pkg expr = do
  bp <- getBuildPlan snap
  let mpkgplan = Data.Map.Strict.lookup (PackageName pkg) $ bpPackages bp
  putStrLn $ maybe "Package not found" expr mpkgplan

buildplanPackage :: Snapshot -> Pkg -> IO ()
buildplanPackage snap pkg =
  evalPackageBuildPlan snap pkg (showVersion . ppVersion)

buildplanUsers :: Snapshot -> Pkg -> IO ()
buildplanUsers snap pkg =
  evalPackageBuildPlan snap pkg (unwords . Set.elems . Set.map unPackageName . ppUsers)

buildplanGithub :: Snapshot -> Pkg -> IO ()
buildplanGithub snap pkg =
  evalPackageBuildPlan snap pkg (unwords . Set.elems . Set.map T.unpack . ppGithubPings)

projectToSnap :: Project -> Snapshot
projectToSnap Nightly = LatestNightly
projectToSnap LTS = LatestLTS

buildplanLatest :: Project -> IO ()
buildplanLatest prj = do
  latest <- findBuildPlanYaml True (projectToSnap prj)
  putStrLn $ takeBaseName latest

buildplanUpdate :: Project -> IO ()
buildplanUpdate project =
  getProjectDir project >>= updateProject True

buildplanConstraints :: Snapshot -> Pkg -> IO ()
buildplanConstraints snap pkg =
  evalPackageBuildPlan snap pkg (show . ppConstraints)

buildplanDependencies :: Snapshot -> Pkg -> IO ()
buildplanDependencies snap pkg =
  evalPackageBuildPlan snap pkg (unlines . map unPackageName . Data.Map.Strict.keys . sdPackages . ppDesc)

buildplanExecutables :: Snapshot -> Pkg -> IO ()
buildplanExecutables snap pkg =
  evalPackageBuildPlan snap pkg (unlines . map (T.unpack . unExeName) . Set.toList. sdProvidedExes . ppDesc)

buildplanPkgTools :: Snapshot -> Pkg -> IO ()
buildplanPkgTools snap pkg =
  evalPackageBuildPlan snap pkg (unlines . map (T.unpack . unExeName) . Data.Map.Strict.keys . sdTools . ppDesc)

buildplanModules :: Snapshot -> Pkg -> IO ()
buildplanModules snap pkg =
  evalPackageBuildPlan snap pkg (unlines . map T.unpack . Set.toList. sdModules . ppDesc)

