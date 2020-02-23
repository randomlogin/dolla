{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import Data.ByteString.Lazy.Char8 (unpack)
import Network.HTTP.Client (method, parseRequest)
import Network.HTTP.Simple (httpLBS, getResponseBody)

import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import Streamly (asyncly, parallely)

import Parse
import Data.Either (rights)
import Data.Maybe (fromJust)
import Data.Map (fromListWith, toList)
import Data.List (sortBy)

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.Data.Stream.StreamK.Type as T 



--retrieves top stories
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


main = do
    hSetBuffering stdout LineBuffering
    putStrLn "Top 30 stories are: "
    topStories <- takeTopStories --retrieving top stories

    --in fact, here instead of simultaneously printing titles of top stories and then processing them I make and
    --additional query to get their titles
    S.drain $ asyncly $ S.mapM (print . fromJust . title) $ S.mapM getItem $ S.fromList $ take 30 topStories

    putStrLn "\nTop 10 commenters are:"
    let top30 = map (\x -> Left x) $ take 30 topStories --taking top 30, making list of Left

    unsortedCommenters <- S.toList $ asyncly $ S.concatMapTreeWith async idToAuthorAndKids $ S.fromList $ top30 --instead of transforming into list, it's possible to filter by Right using streamly, but the difference is neglectable
    let topCommenters = sortedByOccurences $ filter (/= "") $ rights unsortedCommenters --removing empty author of comment (deleted comment)
    mapM_ putStrLn $ map show $ take 10 topCommenters


