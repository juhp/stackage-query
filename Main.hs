{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}

import Control.Monad
import Data.Char (isDigit)
import Data.List
import qualified Data.Map.Strict (assocs, keys, lookup)
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Version
import Options.Applicative
import System.Directory
import System.FilePath
import Text.ParserCombinators.ReadP (readP_to_S)

import Stackage.Types hiding (unPackageName)
import Data.Foldable (traverse_)
import Data.Yaml hiding (Parser)
import Distribution.Package (PackageName, unPackageName)
import Distribution.Text (disp)

import SimpleCmd (cmd, cmd_)
import SimpleCmdArgs

import Paths_stackage_query (version)

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

consumeParser :: Parser Int
consumeParser =
  optionalWith auto 'm' "minimum" "THRESHOLD" "Show packages with at least THRESHOLD consumers (default 5)" 5

parsePkg :: Parser Pkg
parsePkg = strArg "PKG"

parseSnap :: Parser Snapshot
parseSnap = argument auto $ metavar "SNAP"

parseProject :: Parser Project
parseProject = argument auto $ metavar "PROJECT"

main :: IO ()
main =
  simpleCmdArgs (Just version) "Stackage query tool" "" $
  subcommands
    [ Subcommand "package" "Show version of PKG in SNAP" $
      buildplanPackage <$> parseSnap <*> parsePkg
    , Subcommand "users" "Revdeps for PKG in SNAP" $
      buildplanUsers <$> parseSnap <*> parsePkg
    , Subcommand "github" "Stackage owners for PKG in SNAP" $
      buildplanGithub <$> parseSnap <*> parsePkg
    , Subcommand "constraints" "Stackage constraints for PKG in SNAP" $
      buildplanConstraints <$> parseSnap <*> parsePkg
    , Subcommand "dependencies" "Dependencies for PKG in SNAP" $
      buildplanDependencies <$> parseSnap <*> parsePkg
    , Subcommand "executables" "Executables of PKG in SNAP" $
      buildplanExecutables <$> parseSnap <*> parsePkg
    , Subcommand "modules" "Modules of PKG in SNAP" $
      buildplanModules <$> parseSnap <*> parsePkg
    , Subcommand "latest" "Latest snap for PROJECT (nightly or lts)" $
      buildplanLatest <$> parseProject
    , Subcommand "ghc" "GHC version for SNAP" $
      buildplanGHC <$> parseSnap
    , Subcommand "core" "GHC core libraries for SNAP" $
      buildplanCore <$> parseSnap
    , Subcommand "tools" "Tools for SNAP" $
      buildplanTools <$> parseSnap <*> optional parsePkg
    , Subcommand "packages" "All packages in SNAP" $
      buildplanPackages <$> parseSnap
    , Subcommand "consumers" "No of users of packages in SNAP" $
      buildplanConsumers <$> consumeParser <*> parseSnap
    , Subcommand "config" "Download cabal.config file for SNAP" $
      stackageConfig <$> parseSnap
    ]

topurl :: String
topurl = "https://www.stackage.org/"

stackageConfig :: Snapshot -> IO ()
stackageConfig snap = do
  let url = topurl ++ show snap </> "cabal.config"
  cmd_ "curl" ["-L", "-O", url]

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
  dir <- getProjectDir $ snapProject snap
  updateProject False dir
  findSnap dir snap

getBuildPlan :: Snapshot -> IO BuildPlan
getBuildPlan snap = do
  ebp <- findBuildPlanYaml snap >>= decodeFileEither
  either (error . prettyPrintParseException) return ebp

snapProject :: Snapshot -> Project
snapProject LatestLTS = LTS
snapProject (LtsSnap _) = LTS
snapProject (LtsMajor _) = LTS
snapProject _ = Nightly

findSnap :: FilePath -> Snapshot -> IO FilePath
findSnap dir snap = do
  fs <-  sortProject . filter (show snap `isPrefixOf`) <$> getDirectoryContents dir
  if null fs
    then error $ "Snap " ++ show snap ++ " not found"
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

git :: FilePath -> String -> [String] -> IO String
git dir c args =
  cmd "git" $ ["-C", dir] ++ c:args

git_ :: FilePath -> String -> [String] -> IO ()
git_ dir c args =
  cmd_ "git" $ ["-C", dir] ++ c:args

cloneProject :: FilePath -> Project -> IO ()
cloneProject dir proj = do
  let url = "git://github.com/commercialhaskell" </> show proj
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

buildplanConsumers :: Int -> Snapshot -> IO ()
buildplanConsumers threshold snap = do
  bp <- getBuildPlan snap
  traverse_ putPkgConsumption $ Data.Map.Strict.assocs $ ppUsers <$> bpPackages bp
  where
    putPkgConsumption :: (PackageName, Set.Set PackageName) -> IO ()
    putPkgConsumption (p, users) = do
      let n = length users
      when (n >= threshold) $
        putStrLn $ show n ++ " " ++ unPackageName p

evalPackageBuildPlan :: Snapshot -> Pkg -> (PackagePlan -> String) -> IO ()
evalPackageBuildPlan snap pkg expr = do
  bp <- getBuildPlan snap
  let mpkgplan = Data.Map.Strict.lookup (mkPackageName pkg) $ bpPackages bp
  putStrLn $ maybe (error "Package not found") expr mpkgplan

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
  latest <- findBuildPlanYaml (projectToSnap prj)
  putStrLn $ takeBaseName latest

buildplanConstraints :: Snapshot -> Pkg -> IO ()
buildplanConstraints snap pkg =
  evalPackageBuildPlan snap pkg (show . disp . pcVersionRange . ppConstraints)

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

