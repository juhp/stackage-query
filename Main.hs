{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import Data.Maybe
import Data.Monoid
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Simple
import Options.Applicative
import System.Exit
import System.FilePath

data Args = Args Options Command

newtype Options = Options {showurl :: Bool}

data Command = List String [String]
             | Config String

optParser :: Parser Options
optParser = Options <$> switch
  (long "url" <> short 'u' <> help "Show url")

parseString :: String -> Parser String
parseString lbl = strArgument $ metavar lbl

listParser :: Parser Command
listParser = List <$> parseString "SNAP" <*> some (parseString "PKG...")

configParser :: Parser Command
configParser = Config <$> parseString "SNAP"

commandParser :: Parser Command
commandParser =
  subparser $
  command "list" (info listParser (progDesc "Show Stackage SNAP version of PKGs")) <>
  command "config" (info configParser (progDesc "Download cabal.config file for SNAP"))

argsParser :: Parser Args
argsParser = Args <$> optParser <*> commandParser

main :: IO ()
main = do
  cmd <- customExecParser p (info (helper <*> argsParser) $ progDesc "Stackage query tool")
  run cmd
  where
    run (Args opts cmd) =
      case cmd of
        List s ps | checkSnap s -> stackageList opts s ps
        Config s | checkSnap s -> stackageConfig opts s
        _ -> error "SNAP should be start with 'lts' or 'nightly'"
    p = prefs showHelpOnEmpty

checkSnap :: String -> Bool
checkSnap s =
  s `elem` streams ||
  any (`isPrefixOf` s) (map (++ "-") streams)
  where
    streams = ["lts", "nightly"]

topurl :: String
topurl = "https://www.stackage.org/"

stackageList :: Options -> String -> [String] -> IO ()
stackageList opts s ps = do
  mgr <- newManager tlsManagerSettings
  mapM_ (sendReq mgr) ps
  where
    sendReq mgr p = do
      let pkgurl = topurl ++ s ++ "/package/"
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

    giveup u = die $ u ++ " not found in " ++ s


stackageConfig :: Options -> String -> IO ()
stackageConfig opts snap = do
  let url = topurl ++ snap ++ "/cabal.config"
  req <- parseRequest url
  response <- httpLBS req
  L.writeFile "cabal.config" $ getResponseBody response
  when (showurl opts) $
    putStrLn url
