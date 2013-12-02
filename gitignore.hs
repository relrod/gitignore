{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Aeson
import Data.Char (toLower)
import Data.List (dropWhileEnd, find, sort)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as C8
import Network (withSocketsDo)
import Network.HTTP.Conduit hiding (path)
import Safe (headMay)
import System.Environment (getArgs)

data GitIgnoreIndex = GitIgnoreIndex {
    gitIgnores :: [GitIgnore]
 } deriving Show

data GitIgnore = GitIgnore {
    path :: String
  , url :: String
} deriving Show

data GitIgnoreFile = GitIgnoreFile {
    content :: B.ByteString
}

instance FromJSON GitIgnoreIndex where
  parseJSON (Object v) = GitIgnoreIndex <$>
                         v .: "tree"

instance FromJSON GitIgnore where
  parseJSON (Object v) = GitIgnore <$>
                         v .: "path" <*>
                         v .: "url"

instance FromJSON GitIgnoreFile where
  parseJSON (Object v) = GitIgnoreFile <$>
                         v .: "content"

getIgnoresIndex :: IO [GitIgnore]
getIgnoresIndex = withSocketsDo $ do
  x <- parseUrl "https://api.github.com/repos/github/gitignore/git/trees/master"
  let r = x { requestHeaders = [("User-Agent", "gitignore-hs")] }
  l <- withManager $ httpLbs r
  let decodedBody = decode (responseBody l) :: Maybe GitIgnoreIndex
  case decodedBody of
    Nothing -> error "JSON decode failed"
    Just g -> return $ gitIgnores g

getIgnoreFile :: String -> IO B.ByteString
getIgnoreFile url = withSocketsDo $ do
  x <- parseUrl url
  let r = x { requestHeaders = [("User-Agent", "gitignore-hs")] }
  l <- withManager $ httpLbs r
  let decodedBody = decode (responseBody l) :: Maybe GitIgnoreFile
  case decodedBody of
    Nothing -> error "JSON decode failed"
    Just g -> return $ Base64.decodeLenient $ content g

main :: IO ()
main = do
  a <- getArgs
  case headMay a of
    Nothing -> do
      putStrLn "Get gitignore files from GitHub's gitignore repository."
      error "Usage: gitignore [<language> | list] > .gitignore"
    Just s -> do
      x <- getIgnoresIndex
      let ignores =
            fmap (\x -> x { Main.path = fmap toLower (init $ Main.path x) }) $
            filter ((/= "") . Main.path) $
            fmap (\x -> x { Main.path = dropWhileEnd (/= '.') (Main.path x) }) x
      case s of
        "list" ->
          mapM_ putStrLn $ sort $ fmap path ignores
        _ -> do
          let selected = find (\y -> fmap toLower (Main.path y) == s) ignores
          case selected of
            Nothing -> error "No such gitignore was found. Use 'list' for all possible gitignores."
            Just f -> do
              decoded <- getIgnoreFile $ url f
              putStr $ C8.unpack decoded
