module Lib
    ( someFunc
    ) where



data Tape = T [Char] deriving(Read, Show)


data Program = P Char Char Char


data Head = H [Program] Tape



someFunc :: IO ()
someFunc = putStrLn "someFunc"
