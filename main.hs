{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import Data.ByteString.Lazy.Char8 (unpack)
import Network.HTTP.Client (method, parseRequest)
import Network.HTTP.Simple (httpLBS, getResponseBody)

import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import Streamly (asyncly, parallely, async, aheadly)

import Parse
import Data.Either (rights)
import Data.Maybe (fromJust)
import Data.Map (fromListWith, toList)
import Data.List (sortBy)

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.Data.Stream.StreamK.Type as T 

--retrieves top stories ids
takeTopStories :: IO [Int]
takeTopStories = do
    let link = "https://hacker-news.firebaseio.com/v0/topstories.json"
    initReq <- parseRequest link
    let req = initReq { method = "GET" }
    response <- httpLBS req
    let stories = read $ unpack $ getResponseBody response :: [Int]
    return stories


--making an https request of a comment by id, outputs item
getItem :: Int -> IO Item
getItem itemId  = do
    let link = "https://hacker-news.firebaseio.com/v0/item/" ++ show itemId ++ ".json"
    initReq <- parseRequest link
    let request = initReq { method = "GET" }
    response <- httpLBS request
    let item = case Parse.decodeItem $ getResponseBody response of 
                   Nothing -> Parse.emptyItem
                   Just a -> a
    return item

--takes an item and produces a stream of its author and kids
itemToAuthorAndKids :: (T.IsStream t) => Item -> t IO (Either Int String)
itemToAuthorAndKids item = S.fromList $ nickname:children
           where nickname = case by item of
                  Nothing -> Right "" --deleted comment is represented by an empty string
                  Just a -> Right a
                 children = case kids item of 
                  Nothing -> []
                  Just b -> map (\x -> Left x) b

--takes id of the comment, produces stream of its author and subcomments
idToAuthorAndKids :: (T.IsStream t) => Int -> t IO (Either Int String)
idToAuthorAndKids int = S.concatMap itemToAuthorAndKids $ S.mapM getItem $ S.yield int


--counts occurences of an element in a list
frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

--sorting by the most frequent first
sortedByOccurences xs = sortBy (\x y -> compare (snd y) (snd x)) $ frequency xs

printTitleReturnItem it = do 
    putStrLn $ fromJust $ title it
    return it

--How it works:
--First, pulling identificators of top 30 stories, next it's needed to print their titles, to do so it's needed to make
--a query to each story and retrieve it titiles. It's done via streamly without discarding the result of query, so it
--can be used later. Not the most beautiful code, but it's better than to make the same query twice (once for titles,
--once for aceessing its subcomments).
    
--Next the resulting stream is iteratevily processed by accessing children of the current iteration.
main = do
    hSetBuffering stdout LineBuffering
    putStrLn "Top 30 stories are: "
    topStories <- takeTopStories --retrieving top stories

    unsortedCommenters <- S.toList $ asyncly $
        S.concatMapTreeWith async idToAuthorAndKids $ -- 4. process the stream of author and subcomments untile there are no subcomments
        S.concatMap itemToAuthorAndKids $ -- 3. transform item stream into stream of author and subcomments
        S.mapM printTitleReturnItem $ -- 2. print titles of items 
        S.mapM getItem $ S.fromList $ take 30 topStories  -- 1. retrieve items by given top 30 id

    let topCommenters = sortedByOccurences $ filter (/= "") $ rights unsortedCommenters --removing empty author of comment (deleted comment)
    putStrLn "\nTop 10 commenters are:"
    mapM_ putStrLn $ map show $ take 10 topCommenters
