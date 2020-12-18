{-
    Main.hs - Console program to run a Turing machine

    (C) 2020 Tim Gravert <crazymind102@googlemail.com>

    License: BSD 3-Clause
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.TuringMachine

import           Control.Exception
import           Control.Monad
import           Data.Either
import           Data.Maybe
import           Data.Semigroup ((<>))
import           GHC.Generics
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

data Command = Run FilePath | Step FilePath | Count FilePath

runC :: Parser Command
runC = Run
  <$> argument str
    ( metavar "FILE"
    )

stepC :: Parser Command
stepC = Step
  <$> argument str
    ( metavar "FILE"
    )

countC :: Parser Command
countC = Count
  <$> argument str
    ( metavar "FILE"
    )


turingMachineParser :: String -> Parser TuringMachineParser
turingMachineParser st = TuringMachineParser
  <$> subparser
    ( command "run" (info runC ( progDesc "Run the Turing machine through all steps" ))
    <> command "step" (info stepC ( progDesc "Take one step with the Turing machine"))
    <> command "count" (info countC ( progDesc "Run the Turing machine and count the number of ones; Mostly for Busy Beavers or similar programs"))
    )
  <*> strOption
    ( long "start-state"
    <> short 's'
    <> metavar "STATE"
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
    <> help "Whether to print every step or just run through the program."
    )

fileHandler :: FilePath -> IO Input
fileHandler f = do
  file <- Y.decodeFileEither f :: IO (Either Y.ParseException Input)
  case file of
    Left e -> putStrLn ("When reading the input file we encountered exception " ++ show e) >>= \_ -> exitWith (ExitFailure 1)
    Right x -> return x

tmParser :: TuringMachineParser -> IO (TuringMachine,FilePath,Maybe Integer)

tmParser (TuringMachineParser (Run f) "" True _) = do
  file <- fileHandler f
  let tmf = input file
  return ((run (TM (fstT tmf) (sndT tmf) (trdT tmf) (frtT tmf))),f,Nothing)
tmParser (TuringMachineParser (Run f) "" False False) = do
  file <- fileHandler f
  let tmf = input file
  let tm = TM (fstT tmf) (sndT tmf) (trdT tmf) (frtT tmf)
  putStrLn ((show tm) ++ "\n")
  let ntm = run tm
  putStrLn (show ntm)
  return (ntm,f,Nothing)
tmParser (TuringMachineParser (Run f) "" False True) = do
  file <- fileHandler f
  let tmf = input file
  let tm = TM (fstT tmf) (sndT tmf) (trdT tmf) (frtT tmf)
  putStrLn (show tm)
  ntm <- runP tm
  return (ntm,f,Nothing)

tmParser (TuringMachineParser (Step f) "" True _) = do
  file <- fileHandler f
  let tmf = input file
  let tm = TM (fstT tmf) (sndT tmf) (trdT tmf) (frtT tmf)
  return ((step tm),f,Nothing)
tmParser (TuringMachineParser (Step f) "" False _) = do
  file <- fileHandler f
  let tmf = input file
  let tm = TM (fstT tmf) (sndT tmf) (trdT tmf) (frtT tmf)
  putStrLn ((show tm) ++ "\n")
  let ntm = step tm
  putStrLn (show ntm)
  return (ntm,f,Nothing)

tmParser (TuringMachineParser (Count f) "" True _) = do
  file <- fileHandler f
  let tmf = input file
  let tm = run (TM (fstT tmf) (sndT tmf) (trdT tmf) (frtT tmf))
  let output = Just (count1s tm)
  return (tm,f,output)
tmParser (TuringMachineParser (Count f) "" False False) = do
  file <- fileHandler f
  let tmf = input file
  let tm = TM (fstT tmf) (sndT tmf) (trdT tmf) (frtT tmf)
  putStrLn ((show tm) ++ "\n")
  let ntm = run tm
  let output = Just (count1s ntm)
  putStrLn (show ntm)
  putStrLn ("\nOnes: " ++ (show (fromJust output)))
  return (ntm,f,output)
tmParser (TuringMachineParser (Count f) "" False True) = do
  file <- fileHandler f
  let tmf = input file
  let tm = TM (fstT tmf) (sndT tmf) (trdT tmf) (frtT tmf)
  putStrLn (show tm)
  tm <- runP tm
  let output = Just (count1s tm)
  putStrLn ("\nOnes: " ++ (show (fromJust output)))
  return (tm,f,output)

tmParser (TuringMachineParser (Run f) st True _) = do
  file <- fileHandler f
  let tmf = input file
  return ((run (TM (T.pack st) (sndT tmf) (trdT tmf) (frtT tmf))),f,Nothing)
tmParser (TuringMachineParser (Run f) st False False) = do
  file <- fileHandler f
  let tmf = input file
  let tm = TM (T.pack st) (sndT tmf) (trdT tmf) (frtT tmf)
  putStrLn ((show tm) ++ "\n")
  let ntm = run tm
  putStrLn (show ntm)
  return (ntm,f,Nothing)
tmParser (TuringMachineParser (Run f) st False True) = do
  file <- fileHandler f
  let tmf = input file
  let tm = TM (T.pack st) (sndT tmf) (trdT tmf) (frtT tmf)
  putStrLn ((show tm) ++ "\n")
  ntm <- runP tm
  return (ntm,f,Nothing)

tmParser (TuringMachineParser (Step f) st True _) = do
  file <- fileHandler f
  let tmf = input file
  return ((step (TM (T.pack st) (sndT tmf) (trdT tmf) (frtT tmf))),f,Nothing)
tmParser (TuringMachineParser (Step f) st False _) = do
  file <- fileHandler f
  let tmf = input file
  let tm = TM (T.pack st) (sndT tmf) (trdT tmf) (frtT tmf)
  putStrLn ((show tm) ++ "\n")
  let ntm = step tm
  putStrLn (show ntm)
  return (ntm,f,Nothing)

tmParser (TuringMachineParser (Count f) st True _) = do
  file <- fileHandler f
  let tmf = input file
  let tm = run (TM (T.pack st) (sndT tmf) (trdT tmf) (frtT tmf))
  let output = Just (count1s tm)
  return (tm,f,output)
tmParser (TuringMachineParser (Count f) st False False) = do
  file <- fileHandler f
  let tmf = input file
  let tm = TM (T.pack st) (sndT tmf) (trdT tmf) (frtT tmf)
  putStrLn ((show tm) ++ "\n")
  let ntm = run tm
  let output = Just (count1s ntm)
  putStrLn (show ntm)
  putStrLn ("\nOnes: " ++ (show (fromJust output)))
  return (ntm,f,output)
tmParser (TuringMachineParser (Count f) st False True) = do
  file <- fileHandler f
  let tmf = input file
  let tm = TM (T.pack st) (sndT tmf) (trdT tmf) (frtT tmf)
  putStrLn (show tm)
  ntm <- runP tm
  putStrLn (show ntm)
  let output = Just (count1s ntm)
  putStrLn ("\nOnes: " ++ (show (fromJust output)))
  return (ntm,f,output)


data Input = Input
  { state :: State
  , table :: M.Map T.Text (M.Map Char (Char,Move,T.Text))
  , tape  :: (T.Text,Char,T.Text)
  , steps :: Integer
  } deriving(Generic,Read,Show)

instance Y.FromJSON Input
instance Y.ToJSON Input
instance Y.FromJSON Move
instance Y.ToJSON Move

input :: Input -> (State,Table,Tape,Integer)
input (Input state table tape steps) = (state,(toTable table), (toTape tape),steps)
  where
    toTable x = Table x
    toTape (xs,y,zs) = Tape xs y zs

toInput :: TuringMachine -> Input
toInput (TM st (Table ta) (Tape xs y zs) n) = Input st ta ts n
  where
    ts = (xs,y,zs)


fstT (x,_,_,_) = x
sndT (_,x,_,_) = x
trdT (_,_,x,_) = x
frtT (_,_,_,x) = x
fstO (x,_,_) = x
sndO (_,x,_) = x
trdO (_,_,x) = x


main :: IO ()
main = do
  let st = ""
  tmParsed <- tmParser =<< execParser (opts st)
  Y.encodeFile ((sndO tmParsed) <.> "result") (toInput (fstO tmParsed))
  when (isJust (trdO tmParsed)) (writeFile ((sndO tmParsed) <.> "1s") ((show (fromJust (trdO tmParsed))) ++ "\n"))
  exitSuccess
    where
      opts st = info (turingMachineParser st <**> helper)
        ( fullDesc
        <> progDesc "Run or take a step in a Turing Machine"
        <> header "turing-machine - a Turing Machine"
        )
