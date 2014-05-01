{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}

module Types where

import Control.Applicative
import Data.Aeson
import Data.Time

import qualified Data.Map as M

type Chains = M.Map String Chain

data Chain = Chain {
    chainName     :: String
,   chainStart    :: UTCTime
,   chainEnd      :: Maybe UTCTime
,   chainRunning  :: Bool
,   chainProgress :: [Bool]
} deriving (Show)

parseProgress :: [(Int, Bool)] -> [Bool]
parseProgress = map snd

encodeProgress :: [Bool] -> [(Int, Bool)]
encodeProgress progress = zip (iterate succ 0) progress 

instance FromJSON Chain where
    parseJSON (Object v) = Chain <$>
                           v .: "name"     <*>
                           v .: "start"    <*>
                           v .: "end"      <*>
                           v .: "running"  <*>
                           fmap parseProgress (v .: "progress")

instance ToJSON Chain where
    toJSON Chain{..} = object [
                            "name"     .= chainName
                        ,   "start"    .= chainStart
                        ,   "end"      .= chainEnd
                        ,   "running"  .= chainRunning
                        ,   "progress" .= (encodeProgress chainProgress)
                       ]