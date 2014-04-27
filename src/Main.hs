{-# LANGUAGE RecordWildCards, LambdaCase #-}

module Main where 

import Control.Monad.State
import Data.Maybe
import Data.List (intersperse)
import System.Environment

type Chains = [Chain]

data Chain = Chain {
    chainName  :: String
,   chainStart :: Int
,   chainEnd   :: Int
,   chainRunning :: Bool
,   chainProgress :: [Bool]
} deriving (Show, Read)

maybeRead = fmap fst . listToMaybe . reads

process :: [String] -> StateT Chains IO ()
process (cmd:args) |  cmd == "add"  = addChain args
                   |  cmd == "show" = showChains
                   |  cmd == "edit" = editChains

editChains = undefined
loadChains = fmap maybeRead $ readFile "chains"

addChain :: [String] -> StateT Chains IO ()
addChain (name:_) = do
    chains <- get
    let chain = Chain name 1 0 True []
    put $ chains ++ [chain]

showChains :: StateT Chains IO ()
showChains = do
    chains <- get
    let r = map showChain chains
    liftIO $ print r

showChain :: Chain -> [String]
showChain Chain{..} = [chainName,"\n",show chainStart,"\n",progress]
  where
    progress = concat $ 
               intersperse "\n" $ 
               map (\x -> if x then "V" else "X") chainProgress

main :: IO ()
main = do
    args <- getArgs 
    chains  <- loadChains >>= \case
        Just chains -> execStateT (process args) chains 
        Nothing     -> execStateT (process args) []
    writeFile "chains" $ show chains


