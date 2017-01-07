{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment

streams = ["lts", "nightly"]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [s] | s `elem` streams -> showSnap s
    [p] -> query "nightly" p
    [s, p] -> query s p
    _ -> error "Usage: stackage-query [snap] [pkg]"

query snap pkg = do
  mgr <- newManager tlsManagerSettings
  req <- parseRequest $ "http://www.stackage.org/" ++ snap ++ "/package/" ++ pkg
  r <- responseOpenHistory req mgr
  B.putStrLn $ last . catMaybes . map (lookup "Location" . responseHeaders . snd) $ hrRedirects r

showSnap s = do
  mgr <- newManager tlsManagerSettings
  req <- parseRequest $ "http://www.stackage.org/" ++ s
  r <- responseOpenHistory req mgr
  B.putStrLn $ last . catMaybes . map (lookup "Location" . responseHeaders . snd) $ hrRedirects r
