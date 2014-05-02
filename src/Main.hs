module Main where 

import System.Environment

import Interfaces.Cli
import Interfaces.Web

main :: IO ()
main = do
    (arg:args) <- getArgs 
    case arg of 
      "web" -> web
      _     -> cli $ arg:args