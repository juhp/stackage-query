{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Simple
import Options.Applicative
import System.Environment
import System.Exit

data Command = List String [String]
             | Config String

parseString :: String -> Parser String
parseString lbl = strArgument $ metavar lbl

listParser :: Parser Command
listParser = List <$> parseString "SNAP" <*> some (parseString "PKG...")

configParser :: Parser Command
configParser = Config <$> parseString "SNAP"

commandParser :: ParserInfo Command
commandParser = info (helper <*> commands) $ progDesc "try --help for more info" where
    commands = subparser $ mconcat
      [ command "list"
        (info listParser (progDesc "Show Stackage SNAP version of PKGs"))
      , command "config"
        (info configParser (progDesc "Download cabal.config file for SNAP"))
      ]

main :: IO ()
main = do
  cmd <- execParser commandParser
  case cmd of
    List s ps -> stackageRequest s ps
    Config s -> stackageConfig s

stackageRequest s ps = do
  mgr <- newManager tlsManagerSettings
  mapM_ (sendReq mgr) ps
  where
    sendReq mgr p = do
      let path = pkgPath s p
      req <- parseRequest $ url ++ path
      hist <- responseOpenHistory req mgr
      let redirs = mapMaybe (lookup "Location" . responseHeaders . snd) $ hrRedirects hist
      if null redirs
        then giveup path
        else do
        let loc = last redirs
        if url `isPrefixOf` B.unpack loc
          then B.putStrLn loc
          else giveup path

    url = "https://www.stackage.org/"

    giveup path = die $ path ++ " not found"

    pkgPath :: String -> String -> String
    pkgPath s p =
      if checkSnap s
      then s ++ "/package/" ++ p
      else error "SNAP should be start with 'lts' or 'nightly'"

    checkSnap :: String -> Bool
    checkSnap s =
      s `elem` streams ||
      any (`isPrefixOf` s) (map (++ "-") streams)
      where
        streams = ["lts", "nightly"]

stackageConfig snap = do
  req <- parseRequest $ "https://www.stackage.org/" ++ snap ++ "/cabal.config"
  response <- httpLBS req
  L.writeFile "cabal.config" $ getResponseBody response

