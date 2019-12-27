{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.HTTP.Simple     (httpBS, getResponseBody, Request, parseRequest_)
import qualified Data.ByteString.Char8   as B8
import           Data.List

--import           Control.Lens                   ( preview )
--import           Data.Aeson.Lens                ( key, _String )
--import           Data.Text                      ( Text )


fetchIds :: IO B8.ByteString
fetchIds = do
  res <- httpBS "https://hacker-news.firebaseio.com/v0/topstories.json"
  return (getResponseBody res)

parseIds :: B8.ByteString -> [B8.ByteString]
parseIds x = B8.split ',' $ B8.tail $ B8.init x

getNthId :: [B8.ByteString] -> Int -> B8.ByteString
getNthId x n = x!!n

byteStringToString :: B8.ByteString -> String
byteStringToString x = filter (/='"') $ show x

prepareUrl :: String -> Request
prepareUrl x =
  parseRequest_ $ intercalate "" ["https://hacker-news.firebaseio.com/v0/item/", x, ".json"] 

fetchStory :: String -> IO B8.ByteString
fetchStory id = do
  res <- httpBS $ prepareUrl id
  return (getResponseBody res)

main :: IO ()
main = do
  ids <- fetchIds
  story <- fetchStory $ byteStringToString $ getNthId (parseIds ids) 0
  print story
