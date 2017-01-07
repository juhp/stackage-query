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
    [s] | s `elem` streams -> stackageRequest s
    [p] -> stackageRequest $ pkgPath "nightly" p
    [s, p] -> stackageRequest $ pkgPath s p
    _ -> error "Usage: stackage-query [snap] [pkg]"
  where
    pkgPath :: String -> String -> String
    pkgPath s p = s ++ "/package/" ++ p


stackageRequest path = do
  mgr <- newManager tlsManagerSettings
  req <- parseRequest $ "https://www.stackage.org/" ++ path
  hist <- responseOpenHistory req mgr
  B.putStrLn $ last . catMaybes . map (lookup "Location" . responseHeaders . snd) $ hrRedirects hist
