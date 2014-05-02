{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}

module Types where

import Control.Applicative
import Data.Aeson
import Data.Time hiding (parseTime)
import System.Locale

import qualified Data.Map as M

type Chains = M.Map String Chain

data Chain = Chain {
    chainName     :: String
,   chainStart    :: UTCTime
,   chainEnd      :: Maybe UTCTime
,   chainRunning  :: Bool
,   chainProgress :: [Bool]
,   chainStreak   :: Int
} deriving (Show)

parseProgress :: [(Int, Bool)] -> [Bool]
parseProgress = map snd

encodeProgress :: [Bool] -> [(Int, Bool)]
encodeProgress progress = zip (iterate succ 0) progress

parseTime :: String -> UTCTime
parseTime time = readTime defaultTimeLocale "%s" time 

encodeTime :: UTCTime -> String
encodeTime = formatTime defaultTimeLocale "%s"

instance FromJSON Chain where
    parseJSON (Object v) = Chain <$>
                           v .: "name"     <*>
                           fmap parseTime (v .: "start") <*>
                           v .: "end"      <*>
                           v .: "running"  <*>
                           fmap parseProgress (v .: "progress") <*>
                           v .: "streak"

instance ToJSON Chain where
    toJSON Chain{..} = object [
                            "name"     .= chainName
                        ,   "start"    .= encodeTime chainStart
                        ,   "end"      .= chainEnd
                        ,   "running"  .= chainRunning
                        ,   "progress" .= encodeProgress chainProgress
                        ,   "streak"   .= chainStreak
                       ]