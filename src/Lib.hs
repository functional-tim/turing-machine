{-
    Lib.hs - Library to construct and use a Turing machine

    (C) 2020 Tim Gravert <crazymind102@googlemail.com>

    License: BSD 3-Clause
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( count1s
    , Move(L,R)
    , run
    , runP
    , State
    , step
    , stepP
    , Table(Table)
    , Tape(Tape)
    , toMove
    , TuringMachine(TM)
    ) where

import           Data.Char
import           Data.Maybe
import           Data.Yaml
import           GHC.Generics

import qualified Data.Map  as M
import qualified Data.Text as T



-- |The implementation of the Turing machine.
-- The Turing machine has three values: the State it is in, the table of
-- instructions and the tape.
data TuringMachine = TM State Table Tape Integer deriving(Read)

-- |The State of the Turing machine is saved as a String.
type State = T.Text

-- |Implementation of the movement of the head of the Turing machine.
data Move =
    -- |L is used to move the head of the Turing machine to the left.
    L |
    -- |R is used to move the head of the Turing machine to the right.
    R
    deriving(Eq, Generic, Read)

toMove :: T.Text -> Maybe Move
toMove "L" = Just L
toMove "R" = Just R
toMove _ = Nothing

-- |Implementation of the table of instructions.
-- The States of the Turing machine are the keys of a Map.
-- The values of this Map are the instructions.
-- The instructions are another Map.
-- The keys of that Map are the read Symbol.
-- The values of that Map are a tuple which tells the Turing machine what
-- to do.
-- The first element of the tuple is the Symbol to write.
-- The second element is the movement the head has to perform.
-- The third element is the State in which the Turing machine switches
-- afterwads.
data Table = Table (M.Map State (M.Map Char (Char, Move, State))) deriving(Read)

-- |Implementation of the tape of the Turing machine. The Symbols are
-- stored in two Strings and a Char.
-- The String on the left is the tape left of the head and the String on
-- the right is the tape right of the head.
-- The Char is the Symbol the head is over at a given moment.
-- In this implementation of a Turing machine the head is part of the tape
-- itself.
data Tape = Tape T.Text Char T.Text deriving(Read)


-- |Instance of Show for TuringMachine
instance Show TuringMachine where
    show (TM s ta tp n) = "TM\nState: " ++ (T.unpack s) ++ "\n" ++ show ta ++ show tp ++ "Steps: " ++ show n

-- |Instance of Show for Table
instance Show Table where
    show (Table ts)
        | M.null ts = ""
        | otherwise = (T.unpack h) ++ ":  " ++ tst ++ "\n" ++ (show (Table nts))
            where h   = head (M.keys ts)
                  tst = showSymbols (M.toList (ts M.! h))
                  nts = M.delete h ts

-- showSymbols and its helper function showSymbols' are helpers for the
-- Instance of Show for Table
showSymbols :: [(Char, (Char, Move, T.Text))] -> String
showSymbols [] = []
showSymbols (x:xs) = (showSymbols' x) ++ (showSymbols xs)
showSymbols' :: (Char, (Char, Move, T.Text)) -> String
showSymbols' (x,(y0,y1,y2)) = [x] ++ ": " ++ "(" ++ [y0] ++ ", " ++ (show y1) ++ ", " ++ (T.unpack y2) ++ ")\t"

-- |Instance of Show for Move.
instance Show Move where
    show L = "L"
    show R = "R"

-- |Instance of Show for Tape.
instance Show Tape where
    show (Tape xs y zs) = T.unpack (T.append xs (T.append "[" (T.cons y (T.append "]" zs))))


--instance FromJSON Move
--instance ToJSON Move
-- Helper functions to read out the Table values
fstC :: (Char, Move, State) -> Char
fstC (x, _, _) = x

sndC :: (Char, Move, State) -> Move
sndC (_, x, _) = x

trdC :: (Char, Move, State) -> State
trdC (_, _, x) = x


-- |This function moves the head of the Turing machine.
move :: Move -> Tape -> Tape
move m (Tape lt pos rt)
  | m == L && T.length lt == 0 = Tape (T.pack ['0']) '0' (T.cons pos rt)
  | m == L = Tape (T.init lt) (T.last lt) (T.cons pos rt)
  | m == R && T.length rt == 0 = Tape (T.snoc lt pos) '0' (T.pack ['0'])
  | m == R = Tape (T.snoc lt pos) (T.head rt) (T.tail rt)

-- |Each step of the Turing machine is implemented in this function.
-- It returns the resulting Turing machine.
step :: TuringMachine -> TuringMachine
step (TM "HALT" ta ts n) = TM "HALT" ta ts (n+1)
step (TM s (Table ta) (Tape lt pos rt) n) = TM (trdC card) (Table ta) (move (sndC card) (Tape lt uPos rt)) (n+1)
    where card = (ta M.! s) M.! pos
          uPos = fstC card

-- |Version of step that prints the result.
-- It is intented for only a single step.
-- For a complete run that should be printed use runP.
stepP :: TuringMachine -> IO TuringMachine
stepP tm@(TM "HALT" ta ts n) = return tm
stepP tm@(TM s (Table ta) (Tape lt pos rt) n) = do
    putStrLn ""
    putStr "State: "
    print (getState (step tm))
    putStr "Step: "
    print (n+1)
    print (getTape (step tm))
    return (step tm)
    

-- |Running the Turing machine without any output until the Turing machine
-- halts.
-- It returns the finished Turing machine.
run :: TuringMachine -> TuringMachine
run tm@(TM "HALT" ta ts n) = tm
run tm@(TM s ta ts n) = run (step tm)


-- |Version of `run` that prints the results after each step.
runP :: TuringMachine -> IO TuringMachine
runP tm@(TM "HALT" ta ts n) = return tm
runP tm@(TM s ta ts n) = do
    putStrLn ""
    putStr "State: "
    print (getState (step tm))
    putStr "Step: "
    print (n+1)
    print (getTape (step tm))
    ntm <- runP (step tm)
    return ntm


getTape :: TuringMachine -> Tape
getTape (TM s ta ts n) = ts

getState :: TuringMachine -> State
getState (TM s ta ts n) = s


-- |Function to count the ones on the tape.
-- It is primaly good to be used for Busy Beavers and similar programs.
count1s :: TuringMachine -> Integer
count1s (TM s ta (Tape xs y zs) n) = (foldr (+) 0 nxs) + ny + (foldr (+) 0 nzs)
    where nxs = map (toInteger) (map (digitToInt) (T.unpack xs))
          nzs = map (toInteger) (map (digitToInt) (T.unpack zs))
          ny  = toInteger (digitToInt y)
