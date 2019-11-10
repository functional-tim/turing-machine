{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( Card
    , Move
    , run
    , State
    , step
    , Tape
    , tm0
    , tmB2
    ) where

import qualified Data.Char as C
import qualified Data.Map  as M
import qualified Data.Text as T



data Move = L | R deriving(Eq, Read)

data State = S String | HALT deriving(Eq, Ord, Read)

data Tape = Tape [Integer] Integer [Integer] deriving(Read)

data Card = Card (M.Map Integer (Integer, Move, State)) deriving(Read)

data Table = Table (M.Map State Card) deriving(Read)

data TuringMachine = TM State Table Tape deriving(Read)


instance Show Move where
    show L = "L"
    show R = "R"

instance Show State where
    show HALT = "HALT"
    show (S x) = x ++ "   "

instance Show Tape where
    show (Tape xs y zs) = txs ++ "[" ++ (show y) ++ "]" ++ tzs
        where txs = filter (/='[') (filter (/=',') (filter (/=']') (show xs)))
              tzs = filter (/='[') (filter (/=',') (filter (/=']') (show zs)))

instance Show Card where
    show (Card cs) = "(0," ++ (show (cs M.! 0)) ++ ")|(1," ++ (show (cs M.! 1)) ++ ")"

instance Show Table where
    show (Table ts)
      | M.null ts = ""
      | otherwise = (show h) ++ ": " ++ (show (ts M.! h)) ++ "\n" ++ (show (Table nts))
        where h = head (M.keys ts)
              nts = M.delete h ts
--    show (Table ts) = show (M.toList ts)

instance Show TuringMachine where
    show (TM s ta tp) = "TM\nState: " ++ show s ++ "\n" ++ show ta ++ show tp


fstC :: (Integer, Move, State) -> Integer
fstC (x, _, _) = x

sndC :: (Integer, Move, State) -> Move
sndC (_, x, _) = x

trdC :: (Integer, Move, State) -> State
trdC (_, _, x) = x


readCard :: State -> M.Map State Card -> M.Map Integer (Integer, Move, State)
readCard s cs = readCard1 (cs M.! s)

readCard1 :: Card -> M.Map Integer (Integer, Move, State)
readCard1 (Card c) = c


move :: Move -> Tape -> Tape
move m (Tape lt pos rt)
  | m == L && length lt == 0 = Tape ([0]) 0 (pos:rt)
  | m == L = Tape (init lt) (last lt) (pos:rt)
  | m == R && length rt == 0 = Tape (lt ++ [pos]) 0 ([0])
  | m == R = Tape (lt ++ [pos]) (head rt) (tail rt)


step :: TuringMachine -> TuringMachine
step (TM HALT cs ts) = TM HALT cs ts
step (TM st@(S s) (Table ta) (Tape lt pos rt)) = TM (trdC card) (Table ta) (move (sndC card) (Tape lt uPos rt))
    where card = (readCard st ta) M.! pos
          uPos = fstC card


run :: TuringMachine -> IO ()
run (TM HALT cs ts) = print ts
run tm@(TM st@(S s) (Table ta) ts) = do
    print ts
    let ntm = step tm
    run ntm


getTape :: TuringMachine -> Tape
getTape (TM s cs ts) = ts


tape0 = Tape [0, 0, 0, 0] 0 [0, 0, 0, 0]
tape1 = Tape [0] 0 []

--tapeA = Tape ["0", "0", "0", "0"] "0" ["0", "0", "0", "0"]


card00 = Card (M.fromList [(0, (1, R, (S "A"))), (1, (1, R, (S "B")))])
card01 = Card (M.fromList [(0, (1, R, HALT)), (1, (0, R, HALT))])
table0 = Table (M.fromList [((S "A"), card00), ((S "B"), card01)])

card10 = Card (M.fromList [(0, (1, R, (S "A"))), (1, (1, R, (S "B")))])
card11 = Card (M.fromList [(0, (1, R, (S "C"))), (1, (1, R, (S "C")))])
card12 = Card (M.fromList [(0, (1, R, HALT)), (1, (0, R, HALT))])
table1 = Table (M.fromList [((S "A"), card10), ((S "B"), card11), ((S "C"), card12)])

cardB20 = Card (M.fromList [(0, (1, R, (S "B"))), (1, (1, L, (S "B")))])
cardB21 = Card (M.fromList [(0, (1, L, (S "A"))), (1, (1, R, HALT))])
tableB2 = Table (M.fromList [((S "A"), cardB20), ((S "B"), cardB21)])

tm0 = TM (S "A") table0 tape0
tmB2 = TM (S "A") tableB2 tape0
