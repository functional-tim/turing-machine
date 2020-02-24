{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Semigroup ((<>))
import           Control.Exception
import           Data.Either
import           Data.Maybe
import           GHC.Generics
import           Lib
import           Options.Applicative
import           System.Exit
import           System.FilePath
import           System.IO

import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Data.Yaml       as Y



data TuringMachineParser = TuringMachineParser
    { optCommand :: Command
    , startState :: String
    , quiet      :: Bool
    , print      :: Bool
    }

data Command = Run FilePath | Step FilePath

runC :: Parser Command
runC = Run <$> argument str (metavar "FILE")

stepC :: Parser Command
stepC = Step <$> argument str (metavar "FILE")

turingMachineParser :: String -> Parser TuringMachineParser
turingMachineParser st = TuringMachineParser
    <$> subparser
        ( command "run" (info runC ( progDesc "Run the Turing machine through all steps" ))
        <> command "step" (info stepC ( progDesc "Take one step with the Turing machine"))
        )
    <*> strOption
        ( long "start-state"
        <> short 's'
        <> metavar "START-STATE"
        <> showDefault
        <> value st
        <> help "Which state the Turing machine should start in"
        )
    <*> switch
        ( long "quiet"
        <> short 'q'
        <> help "Whether to be quiet"
        )
    <*> switch
        ( long "print"
        <> short 'p'
        <> help "Wheter to print out the step/s"
        )
--    <*> switch
--        ( long "count"
--        <> short 'c'
--        <> help "Count the ones on the tape"
--        )

fileHandler :: FilePath -> IO Input
fileHandler f = do
  file <- Y.decodeFileEither f :: IO (Either Y.ParseException Input)
  case file of
    Left e -> putStrLn ("When reading the input file we encountered exception " ++ show e) >>= \_ -> exitWith (ExitFailure 1)
    Right x -> return x

tmParser :: TuringMachineParser -> IO (TuringMachine,FilePath)
tmParser (TuringMachineParser (Run f) "" True _) = do
  file <- fileHandler f
  let tmf = input file
  return ((run (TM (fstT tmf) (sndT tmf) (trdT tmf) (frtT tmf))),f)
tmParser (TuringMachineParser (Run f) "" False False) = do
  file <- fileHandler f
  let tmf = input file
  return ((run (TM (fstT tmf) (sndT tmf) (trdT tmf) (frtT tmf))),f)
tmParser (TuringMachineParser (Run f) "" False True) = do
  file <- fileHandler f
  let tmf = input file
  tm <- runP (TM (fstT tmf) (sndT tmf) (trdT tmf) (frtT tmf))
  return (tm,f)
tmParser (TuringMachineParser (Step f) "" True _) = do
  file <- fileHandler f
  let tmf = input file
  return ((step (TM (fstT tmf) (sndT tmf) (trdT tmf) (frtT tmf))),f)
tmParser (TuringMachineParser (Step f) "" False False) = do
  file <- fileHandler f
  let tmf = input file
  return ((step (TM (fstT tmf) (sndT tmf) (trdT tmf) (frtT tmf))),f)
tmParser (TuringMachineParser (Step f) "" False True) = do
  file <- fileHandler f
  let tmf = input file
  tm <- stepP (TM (fstT tmf) (sndT tmf) (trdT tmf) (frtT tmf))
  return (tm,f)
tmParser (TuringMachineParser (Run f) st True _) = do
  file <- fileHandler f
  let tmf = input file
  return ((run (TM (T.pack st) (sndT tmf) (trdT tmf) (frtT tmf))),f)
tmParser (TuringMachineParser (Run f) st False False) = do
  file <- fileHandler f
  let tmf = input file
  return ((run (TM (T.pack st) (sndT tmf) (trdT tmf) (frtT tmf))),f)
tmParser (TuringMachineParser (Run f) st False True) = do
  file <- fileHandler f
  let tmf = input file
  tm <- runP (TM (T.pack st) (sndT tmf) (trdT tmf) (frtT tmf))
  return (tm,f)
tmParser (TuringMachineParser (Step f) st True _) = do
  file <- fileHandler f
  let tmf = input file
  return ((step (TM (T.pack st) (sndT tmf) (trdT tmf) (frtT tmf))),f)
tmParser (TuringMachineParser (Step f) st False False) = do
  file <- fileHandler f
  let tmf = input file
  return ((step (TM (T.pack st) (sndT tmf) (trdT tmf) (frtT tmf))),f)
tmParser (TuringMachineParser (Step f) st False True) = do
  file <- fileHandler f
  let tmf = input file
  tm <- stepP (TM (T.pack st) (sndT tmf) (trdT tmf) (frtT tmf))
  return (tm,f)


data Input = Input
    { state :: State
    , table :: M.Map T.Text (M.Map Char (Char,Move,T.Text))
    , tape  :: (T.Text,Char,T.Text)
    , steps :: Integer
    } deriving(Generic,Read,Show)

instance Y.FromJSON Input
instance Y.ToJSON Input

input :: Input -> (State,Table,Tape,Integer)
input (Input state table tape steps) = (state,(toTable table), (toTape tape),steps)
  where toTable x = Table x
        toTape (xs,y,zs) = Tape xs y zs

fstT (x,_,_,_) = x
sndT  (_,x,_,_) = x
trdT (_,_,x,_) = x
frtT (_,_,_,x) = x

toInput :: TuringMachine -> Input
toInput (TM st (Table ta) (Tape xs y zs) n) = Input st ta ts n
  where ts = (xs,y,zs)


main :: IO ()
main = do
  let st = ""
  tmParsed <- tmParser =<< execParser (opts st)
  Y.encodeFile ((snd tmParsed) <.> "result") (toInput (fst tmParsed))
  exitSuccess
    where
      opts st = info (turingMachineParser st <**> helper)
        ( fullDesc
        <> progDesc "Run or take a step in a Turing Machine"
        <> header "turing-machine - a Turing Machine"
        )
--    file <- Y.decodeFileEither "tmBB4.yaml" :: IO (Either Y.Y.ParseException Input)
--    print file
--    let inputData = input (fromRight (Input "" M.empty ("",' ',"")) file)
--    let state = fstT inputData
--    let table = sndT  inputData
--    let tape = trdT inputData
--    let tm = TM state table tape
--    print tm
--    output <- runP tm
--    putStrLn ""
--    print output
--    let c = count1s output
--    putStr "Ones: "
--    print c
--    putStrLn ""
--    print tmBB2
--    output2 <- runP tmBB2 0
--    putStr "Steps: "
--    print (trdT output2)
--    putStrLn ""
--    print tmBB3
--    output3 <- runP tmBB3 0
--    putStr "Steps: "
--    print (trdT output3)
--    putStrLn ""
--    print tmBB4
--    let output4 = run tmBB4 0
--    print (sndT output4)
--    putStr "Steps: "
--    print (trdT output4)
--    putStrLn ""
--    print tmBB5
--    output5 <- runP tmBB5 0
----    print (sndT output5)
--    putStr "Steps: "
--    print (trdT output5)
--    let c5 = count1s (sndT output5)
--    putStr "Ones: "
--    print c5
