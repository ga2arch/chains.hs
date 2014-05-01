{-# LANGUAGE RecordWildCards, LambdaCase, 
             OverloadedStrings #-}

module Cli.Cli where 

import Prelude hiding (readFile)

import Control.Applicative
import Control.Monad.State
import Data.Aeson
import Data.Char
import Data.Time
import Data.List (intersperse)
import System.Directory
import System.Locale
import System.Environment

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M

import Types

lower = map toLower

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
        let chain = Chain name time Nothing True []
        M.insert (lower name) chain chains    

showChains :: StateT Chains IO ()
showChains = do
    chains <- fmap (M.elems) get
    mapM_ (liftIO . putStrLn . showChain) chains

showChain :: Chain -> String
showChain Chain{..} = 
    (concat $ intersperse "\n" temp) ++ "\n"
  where
    separator = "---"
    start = formatTime defaultTimeLocale "%F" chainStart
    streak = length $ takeWhile (== True) $ reverse chainProgress
    spaces = " "
    progress = map (\case True -> spaces ++ "V" 
                          False -> spaces ++ "X") chainProgress
    temp = [chainName, show streak, separator] ++ progress

doneChain :: [String] -> StateT Chains IO ()
doneChain (name:_) = do
    modify $ \chains -> do
        let c@Chain{..} = chains M.! (lower name)
        M.insert (lower name) 
                 (c { chainProgress = chainProgress ++ [True]}) 
                 chains

rmChain :: [String] -> StateT Chains IO ()
rmChain (name:_) = do
    modify $ \chains -> M.delete (lower name) chains 

loadChains :: IO (Maybe Chains)
loadChains = do 
    exists <- doesFileExist "chains"
    if exists
        then fmap decodeStrict $ B.readFile "chains"
        else writeFile "chains" "" >> loadChains

updateChains :: Chains -> IO Chains
updateChains chains = do 
    timezone <- getCurrentTimeZone
    today <- utcToLocalTime <$> getCurrentTimeZone 
                            <*> getCurrentTime
    return $ M.map (go today timezone) chains
  where 
    go today timezone c@Chain{..} = do
        let start = utcToLocalTime timezone chainStart
        let days = fromIntegral $ diffDays (localDay today) 
                                           (localDay start)
        let missed = take (days - length chainProgress) $ repeat False
        let progress = chainProgress ++ missed
        c { chainProgress = progress }

cli :: [String] -> IO ()
cli args = do
    putStrLn "Chains\n==="
    chains  <- loadChains >>= \case
        Just chains -> updateChains chains >>= execStateT (process args) 
        Nothing     -> execStateT (process args) M.empty
    BL.writeFile "chains" $ encode chains
