{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Client (method, parseRequest)
import Network.HTTP.Simple (httpLBS, getResponseBody)
import Control.Concurrent.Async (mapConcurrently, wait, withAsync)
import Data.Maybe (fromJust)
import Data.Map (fromListWith, toList)
import Data.List (sortBy)
import Parse

import Data.ByteString.Lazy.Char8 (unpack)
--I don't use this website, so I don't know for sure why some of the comments are 'dead', perhaps user score.  Example:
--https://news.ycombinator.com/item?id=22103329 these 'dead' comments are not counted at the main page in the comments
--counter, however I count them.
 
--Also I've encountered null value: https://hacker-news.firebaseio.com/v0/item/22104036.json that's why I defined an
--emptyItem in Parse.hs.

taketops :: IO [Int]
taketops = do
    let link = "https://hacker-news.firebaseio.com/v0/topstories.json"
    initReq <- parseRequest link
    let req = initReq { method = "GET" }
    response <- httpLBS req
    let stories = read $ unpack $ getResponseBody response 
    return stories

--gets an item by id
getItem itemId  = do
    let link = "https://hacker-news.firebaseio.com/v0/item/" ++ show itemId ++ ".json"
    initReq <- parseRequest link
    let request = initReq { method = "GET" }
    response <- httpLBS request
    let item = case Parse.decodeItem $ getResponseBody response of 
                   Nothing -> Parse.emptyItem
                   Just a -> a
    return item

--takes comments of an item and its descendants, returns an IO wrapped list of nicknames
author :: Int -> IO [String]
author itemId = do
    withAsync (getItem itemId) $ \x -> do
    item <- wait x
    let nickname = case by item of
          Nothing -> "" --deleted comment represented by an empty string
          Just a -> a
        children = case kids item of 
          Nothing -> []
          Just a -> a
    remaining <- mapConcurrently author children
    let out = filter (/="") $ nickname:(concat remaining)
    return out


--titles of top stories
titleOfStory storyId = do
    withAsync (getItem storyId) $ \x -> do
    story <- wait x
    let titl = case title story of
            Nothing -> ""
            Just a -> a
    return titl

--counts occurences of an element in a list
frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

--The most frequent first, so the compare order is reversed
sortedByOccurences xs = sortBy (\x y -> compare (snd y) (snd x)) $ frequency xs

main = do 
    putStrLn "Top 30 stories are: \n"
    stories <- taketops
    let top30 = take 30 stories
    withAsync (mapConcurrently titleOfStory top30) $ \x -> do
    topTitles <- wait x
    topComments <- mapConcurrently author top30
    mapM_ putStrLn topTitles
    let topCommenters = take 10 $ sortedByOccurences $ concat topComments
    putStrLn "\n\nTop 10 commenters are:\n"
    mapM_ putStrLn $ map show topCommenters
