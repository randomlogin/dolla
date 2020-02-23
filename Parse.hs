{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse where
import GHC.Generics
import Data.Aeson
import Data.ByteString.Lazy.Internal
import Control.Applicative
import Data.Maybe (fromJust)

--generic item: cooment or story
data Item = Item {
        by :: Maybe String,
        descendants :: Maybe Int,
        id :: Maybe Int,
        parent :: Maybe Int,
        kids :: Maybe [Int],
        score :: Maybe Int,
        time :: Maybe Int,
        title :: Maybe String,
        atype :: Maybe String,
        url :: Maybe String
                 } deriving (Generic, Show, Eq)


instance FromJSON Item where
    parseJSON (Object v) =
        Item   <$> v .:? "by"
               <*> v .:? "descendants"
               <*> v .:? "id"
               <*> v .:? "parent"
               <*> v .:? "kids"
               <*> v .:? "score"
               <*> v .:? "time"
               <*> v .:? "title"
               <*> v .:? "type"
               <*> v .:? "url"
    parseJSON _ = empty

decodeItem a = decode a :: Maybe Item


emptyJSON :: Data.ByteString.Lazy.Internal.ByteString
emptyJSON = "{ \"by\" : \"\" }"

emptyItem = fromJust $ decodeItem emptyJSON
