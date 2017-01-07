{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Maybe
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [p] -> stackageRequest $ pkgPath "nightly" p
    [s, p] | checkSnap s -> stackageRequest $ pkgPath s p
    _ -> error "Usage: stackage-query [lts|nightly] [pkg]"
  where
    pkgPath :: String -> String -> String
    pkgPath s p = s ++ "/package/" ++ p

    checkSnap :: String -> Bool
    checkSnap s = s `elem` streams || any (`isPrefixOf` s) (map (++ "-") streams) 

    streams = ["lts", "nightly"]

stackageRequest path = do
  mgr <- newManager tlsManagerSettings
  let url = "https://www.stackage.org/"
  req <- parseRequest $ url ++ path
  hist <- responseOpenHistory req mgr
  let redirs = catMaybes . map (lookup "Location" . responseHeaders . snd) $ hrRedirects hist
  unless (null redirs) $ do
    let loc = last redirs
    when (url `isPrefixOf` B.unpack loc) $
      B.putStrLn loc
