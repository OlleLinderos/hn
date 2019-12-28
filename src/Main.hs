{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.HTTP.Simple   ( httpBS, getResponseBody, Request, parseRequest_ )
import qualified Data.ByteString.Char8 as B8
import           Control.Monad
import           Control.Lens          ( preview )
import           Data.Aeson.Lens       ( key, _String )
import           Data.Text             ( Text )
import qualified Data.Text.IO          as TIO
import           Data.List


fetchIds :: IO B8.ByteString
fetchIds = do
  res <- httpBS "https://hacker-news.firebaseio.com/v0/topstories.json"
  return (getResponseBody res)

parseIds :: B8.ByteString -> [B8.ByteString]
parseIds s = B8.split ',' $ B8.tail $ B8.init s

getNthId :: [B8.ByteString] -> Int -> B8.ByteString
getNthId x n = x!!n

byteStringToString :: B8.ByteString -> String
byteStringToString x = filter (/='"') $ show x

prepareUrl :: String -> Request
prepareUrl id =
  parseRequest_ $ intercalate "" ["https://hacker-news.firebaseio.com/v0/item/", id, ".json"] 

fetchStory :: String -> IO B8.ByteString
fetchStory id = do
  res <- httpBS $ prepareUrl id
  return (getResponseBody res)

getStoryTitle :: B8.ByteString -> Maybe Text
getStoryTitle = preview (key "title" . _String)

getStoryUrl :: B8.ByteString -> Maybe Text
getStoryUrl = preview (key "url" . _String)

main :: IO ()
main = do
  ids   <- fetchIds
  forM_ [1..10] $ \n -> do
         story <- fetchStory $ byteStringToString $ getNthId (parseIds ids) n

         case getStoryTitle story of
           Nothing    -> TIO.putStrLn "Nothing"
           Just title -> TIO.putStrLn $ title

         case getStoryUrl story of
           Nothing    -> TIO.putStrLn "Nothing"
           Just url   -> TIO.putStrLn $ url

