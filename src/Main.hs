module Main where 

import System.Environment

import Interfaces.Cli
import Interfaces.Web

main :: IO ()
main = do
    x <- getArgs
    if length x == 0
        then cli x
        else case x of 
                ("web":_) -> web
                _         -> cli x