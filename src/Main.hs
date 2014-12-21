{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module TTT where

import           Control.Lens
import           Control.Applicative ((<$>))
import           Data.Maybe (mapMaybe)
import           Data.Monoid  ((<>))

data Pl = E | X | O
    deriving (Eq,Ord)

data MoveN = One | Two | Three
    deriving (Show,Eq,Ord)

instance Show Pl where
    show E = "."
    show X = "X"
    show O = "O"

data IllegalMv
  = PlaceTaken
  deriving Show

type Move = (MoveN,MoveN)
type Row = (Int,Pl,Pl,Pl)
type Game = (Row,Row,Row) -- yer boat

mvC :: MoveN -> Lens' Row Pl
mvC One = _2
mvC Two = _3
mvC Three = _4

mvR :: MoveN -> Lens' Game Row
mvR One = _1
mvR Two = _2
mvR Three = _3

readMv :: Char -> Maybe MoveN
readMv s = case s of
  '1' -> Just One
  '2' -> Just Two
  '3' -> Just Three
  _   -> Nothing

parseMove :: String -> Maybe Move
parseMove = (^? _1 . to (\[r,c,_] -> (r,c))) . g
  where
      g = splitAt 2 . mapMaybe readMv

move :: Move -> Lens' Game Pl
move (r,c) = mvR r . mvC c

legalMove :: Game -> Move -> Bool
legalMove g m = g ^. move m . to (==E)

makeMove :: Game -> Pl -> Move -> Game
makeMove g p m = g & move m .~ p

renderRow :: Row -> String
renderRow (n,a,b,c) =
    show n <> " | " <> show a <> " | " <> show b <> " | " <> show c <> " |"

renderGame :: Game -> IO ()
renderGame (a,b,c) =
    putStrLn header >> showR a >> showR b >> showR c
    where
        showR = putStrLn . renderRow
        header = "    1   2   3"

startBoard :: Game
startBoard = ( (1,E,E,E)
             , (2,E,X,E)
             , (3,E,E,E))

victories :: [String]
victories = [
    "11 12 13" -- top row
   ,"11 21 31" -- first column
   ,"11 22 33" -- diagonal from top left
   ,"21 22 23" -- middle column
   ,"31 32 33" -- last column
   ,"31 32 13" -- diagonal from top right
   ]

victoryMoves :: [[Move]]
victoryMoves = f <$> victories
    where
        f = mapMaybe parseMove . words

main :: IO ()
main = do
    let b = startBoard
    renderGame b
    let b' = makeMove b O (One,Two)
    renderGame b'

