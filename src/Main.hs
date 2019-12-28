{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.HTTP.Simple   ( httpBS, getResponseBody, Request, parseRequest_ )
import qualified Data.ByteString.Char8 as B8
import           Control.Monad         ( forM_, when )
import           Control.Lens          ( preview )
import           Data.Aeson.Lens       ( key, _String )
import           Data.Text             ( Text )
import qualified Data.Text.IO          as TIO
import           Data.List
import           System.Console.ANSI
import           System.IO             (stdin, hReady, hSetEcho, hSetBuffering, BufferMode(NoBuffering))


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
  parseRequest_ $ intercalate ""
  ["https://hacker-news.firebaseio.com/v0/item/", id, ".json"] 

fetchStory :: String -> IO B8.ByteString
fetchStory id = do
  res <- httpBS $ prepareUrl id
  return (getResponseBody res)

getStoryTitle :: B8.ByteString -> Maybe Text
getStoryTitle = preview (key "title" . _String)

getStoryUrl :: B8.ByteString -> Maybe Text
getStoryUrl = preview (key "url" . _String)

printStories :: [Int] -> B8.ByteString -> IO ()
printStories r ids = do
  forM_ r $ \n -> do
    let i = show (n+1)
    story <- fetchStory
             $ byteStringToString
             $ getNthId (parseIds ids) n

    putStr $ i ++ ". " 

    case getStoryTitle story of
      Nothing    -> TIO.putStrLn "Nothing"
      Just title -> TIO.putStrLn $ title

    case getStoryUrl story of
      Nothing    -> TIO.putStrLn "Nothing"
      Just url   -> TIO.putStrLn $ url

    putStr "\n"

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

actions :: [Int] -> B8.ByteString -> IO ()
actions n ids = do 
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  key <- getKey
  when (key /= "ESC") $ do
    case key of
      "n" -> do
        clearScreen
        printStories (map (\x -> x + 8) n) ids
        actions (map (\x -> x + 8) n) ids
      "p" -> do
        if n /= [0..7] then do
          clearScreen
          printStories (map (\x -> x - 8) n) ids
          actions (map (\x -> x - 8) n) ids
        else do
          clearScreen
          putStrLn "Hold your horses, you can't go that way."
      _ -> return ()
  
main :: IO ()
main = do
  let r = [0..7]
  ids <- fetchIds
  putStrLn "--Usage--------"
  putStrLn "\"n\" for next page, or \"p\" for previous page."
  putStrLn "---------------\n"
  printStories r ids
  actions r ids
