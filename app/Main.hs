module Main where

import Lib
import System.Exit

main :: IO ()
main = do
    print Lib.tmB2
    run Lib.tmB2
    exitSuccess
