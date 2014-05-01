{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

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

instance FromJSON Chain where
    parseJSON (Object v) = Chain <$>
                           v .: "name"
                           <*> v .: "start"
                           <*> v .: "end"
                           <*> v .: "running"
                           <*> v .: "progress"

instance ToJSON Chain where
    toJSON Chain{..} = object [
                            "name"     .= chainName
                        ,   "start"    .= chainStart
                        ,   "end"      .= chainEnd
                        ,   "running"  .= chainRunning
                        ,   "progress" .= chainProgress
                       ]
