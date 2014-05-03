{-# LANGUAGE RecordWildCards, LambdaCase, 
             OverloadedStrings #-}

module Interfaces.Cli where 

import Prelude hiding (readFile)

import Control.Monad.State
import Data.Aeson
import Data.Time
import Data.List (intersperse)
import System.Locale

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M

import Utils
import Types

process :: [String] -> StateT Chains IO ()
process [] = showChains
process (cmd:args) |  cmd == "add"  = addChain args  >> showChains
                   |  cmd == "show" = showChains
                   |  cmd == "done" = doneChain args >> showChains
                   |  cmd == "rm"   = rmChain args   >> showChains

addChain :: [String] -> StateT Chains IO ()
addChain (name:_) = do
    time <- liftIO getCurrentTime
    modify $ \chains -> do 
        let chain = Chain name time Nothing True [] 0
        M.insert (lower name) chain chains    

showChains :: StateT Chains IO ()
showChains = do
    chains <- fmap (M.elems) get
    mapM_ (liftIO . putStrLn . showChain) chains

showChain :: Chain -> String
showChain Chain{..} = 
    (concat $ intersperse "\n" temp) ++ "\n"
  where
    header = chainName ++ " - " ++ show streak
    separator = take (length header) $ repeat '-'
    start = formatTime defaultTimeLocale "%F" chainStart
    streak = length $ takeWhile (== True) $ reverse chainProgress
    progress = concatMap (\case True -> "V " 
                                False -> "X ") chainProgress
    temp = [header, separator] ++ [progress]

doneChain :: [String] -> StateT Chains IO ()
doneChain (name:_) = do
    modify $ \chains -> do
        let c@Chain{..} = chains M.! (lower name)
        M.insert (lower name) 
                 (c { chainProgress = chainProgress ++ [True]
                    , chainStreak = succ chainStreak }) 
                 chains

rmChain :: [String] -> StateT Chains IO ()
rmChain (name:_) = do
    modify $ \chains -> M.delete (lower name) chains 

cli :: [String] -> IO ()
cli args = do
    putStrLn "Chains\n=====\n"
    chains  <-  loadChains 
            >>= updateChains 
            >>= execStateT (process args)
    BL.writeFile "chains.db" $ encode chains
