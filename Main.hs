{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}

#if (defined(MIN_VERSION_directory) && MIN_VERSION_directory(1,2,3))
#else
import Control.Exception (bracket)
#endif
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit)
import Data.List
import qualified Data.Map.Strict (assocs, keys, lookup)
import Data.Maybe
import Data.Semigroup ((<>))
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Version
import Options.Applicative
import System.Directory
import System.FilePath
import System.IO (hPutStrLn, stderr)
import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)
import Text.ParserCombinators.ReadP (readP_to_S)

import Stackage.Types hiding (unPackageName)
import Data.Foldable (traverse_)
import Data.Yaml hiding (Parser)
import Distribution.Package (PackageName, unPackageName)
import Distribution.Text (disp)
import Network.HTTP.Directory (httpManager, httpRedirect)

import SimpleCmd (cmd_)
import SimpleCmdArgs

import Paths_stackage_query (version)

data Snapshot = LatestNightly | LatestLTS | NightlySnap String | LtsSnap String | LtsMajor String

instance Show Snapshot where
  show LatestNightly = "nightly"
  show LatestLTS = "lts"
  show (NightlySnap date) = "nightly-" <> date
  show (LtsSnap ver) = "lts-" <> ver
  show (LtsMajor ver) = "lts-" <> ver

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

type Pkg = String

consumeParser :: Parser Int
consumeParser =
  optionalWith auto 'm' "minimum" "THRESHOLD" "Show packages with at least THRESHOLD consumers (default 5)" 5

parsePkg :: Parser Pkg
parsePkg = strArg "PKG"

parseSnap :: Parser Snapshot
parseSnap = argument auto $ metavar "SNAP"

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
      buildplanLatest <$> parseSnap
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
  let url = topurl </> show snap </> "cabal.config"
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
  unless haveProj $ createDirectoryIfMissing False projectDir
  return projectDir

getBuildPlan :: Snapshot -> IO BuildPlan
getBuildPlan snap = do
  ebp <- findBuildPlanYaml >>= decodeFileEither
  either (error . prettyPrintParseException) return ebp
  where
    findBuildPlanYaml :: IO FilePath
    findBuildPlanYaml = do
      dir <- getProjectDir $ snapProject snap
      findSnap dir snap

snapProject :: Snapshot -> Project
snapProject LatestLTS = LTS
snapProject (LtsSnap _) = LTS
snapProject (LtsMajor _) = LTS
snapProject _ = Nightly

findSnap :: FilePath -> Snapshot -> IO FilePath
findSnap dir snap = do
  fs <-  sortProject . filter (show snap `isPrefixOf`) <$> getDirectoryContents dir
  if null fs
    then downloadLatest
    else return $ dir </> last fs
  where
    sortProject :: [String] -> [String]
    sortProject =
      if snapProject snap == Nightly
      then sort
      else map ((\v -> "lts-" <> v <.> "yaml") . showVersion) . sort . map (readVersion . takeBaseName . removePrefix "lts-")

    downloadLatest :: IO FilePath
    downloadLatest = do
      latest <- latestSnap snap
      downloadSnap latest
      return $ dir </> show latest <.> "yaml"

    downloadSnap :: Snapshot -> IO ()
    downloadSnap snp = do
      let proj = snapProject snp
      withCurrentDirectory dir $ do
        tty <- queryTerminal stdInput
        let silent = if tty then ["-#"] else ["-s", "-S"]
            yaml = show snp <.> "yaml"
            url = "https://raw.githubusercontent.com/commercialhaskell" </> show proj </> "master" </> yaml
        hPutStrLn stderr $ "Downloading " <> yaml
        cmd_ "curl" $ silent ++ ["-O", url]

readVersion :: String -> Version
readVersion s =
  let vp = readP_to_S parseVersion s in
    if null vp then giveup
    else case last vp of
           (ver,"") -> ver
           _ -> giveup
  where
    giveup = error $ "readVersion: failed to parse " <> s

buildplanGHC :: Snapshot -> IO ()
buildplanGHC snap = do
  bp <- getBuildPlan snap
  let si = bpSystemInfo bp
  putStrLn $ (showVersion . siGhcVersion) si

showPkgVer :: (PackageName, Version) -> String
showPkgVer (p,v) = unPackageName p <> " " <> showVersion v

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
        putStrLn $ show n <> " " <> unPackageName p

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

latestSnap :: Snapshot -> IO Snapshot
latestSnap snap = do
  mgr <- httpManager
  latest <- httpRedirect mgr $ topurl </> show snap
  return $ maybe snap (read . B.unpack . B.tail) latest

buildplanLatest :: Snapshot -> IO ()
buildplanLatest =
  latestSnap >=> print

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

#if (defined(MIN_VERSION_directory) && MIN_VERSION_directory(1,2,3))
#else
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action =
  bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
    setCurrentDirectory dir
    action
#endif
