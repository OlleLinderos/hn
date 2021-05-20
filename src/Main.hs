{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.HTTP.Simple   ( httpBS, getResponseBody, Request, parseRequest_ )
import qualified Data.ByteString.Char8 as B8
import           Control.Monad         ( forM_ )
import           Control.Lens          ( preview )
import           Data.Aeson.Lens       ( key, _String )
import           Data.Text             ( Text )
import qualified Data.Text.IO          as TIO
import           Data.List
import           System.Console.ANSI
import           System.IO             ( stdin, hReady, hSetEcho, hSetBuffering, BufferMode(NoBuffering) )


fetchIds :: IO B8.ByteString
fetchIds = do
  res <- httpBS "https://hacker-news.firebaseio.com/v0/topstories.json"
  return (getResponseBody res)

parseIds :: B8.ByteString -> [B8.ByteString]
parseIds s = B8.split ',' $ B8.tail $ B8.init s

getNthId :: [B8.ByteString] -> Int -> B8.ByteString
getNthId x n = x!!n

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
             $ B8.unpack
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

reset :: IO ()
reset = do
  clearScreen
  setCursorPosition 0 0

actions :: [Int] -> B8.ByteString -> IO ()
actions n ids = do 
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  key <- getKey
  reset
  case key of
    "n" -> do
      reset
      printStories (map (\x -> x + 8) n) ids
      actions (map (\x -> x + 8) n) ids
    "p" -> do
      if n /= [0..7] then do
                       reset
                       printStories (map (\x -> x - 8) n) ids
                       actions (map (\x -> x - 8) n) ids
      else main
    _ -> return ()

main :: IO ()
main = do
  let r = [0..7]
  ids <- fetchIds
  reset
  printStories r ids
  actions r ids
