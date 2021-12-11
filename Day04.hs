module Day04 where

import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Vector as V
import Text.Megaparsec.Debug
import Data.List (partition)
import Vector2D

type Board = Vector2D (Either Int Int)

inputParser :: Parser ([Int], [Vector2D Int])
inputParser = (,) <$> draws' <*> some card'
  where
    draws = L.decimal `sepBy` char ','
    draws' = draws <* string "\n\n"
    card = Vector2D 5 5 . V.fromList . mconcat <$> some line
    card' = card <* (void newline <|> eof)
    line = hspace *> L.decimal `sepBy1` hspace <* newline

drawNumber :: Int -> Board -> Board
drawNumber num brd = flipDrawn <$> brd
  where
    flipDrawn e@(Right _) = e
    flipDrawn e@(Left cur)
      | cur == num = Right num
      | otherwise = e

isWinning :: Board -> Bool
isWinning brd@(Vector2D rows cols _) = winVertical || winHorizontal
  where
    isRowWinning row = cols == length [ () | col <- [0..cols-1], isRight (brd `at` (row, col)) ]
    isColWinning col = rows == length [ () | row <- [0..rows-1], isRight (brd `at` (row, col)) ]
    winVertical = any isColWinning [0..cols-1]
    winHorizontal = any isRowWinning [0..rows-1]

stage1 :: [Int] -> [Board] -> (Int, Board)
stage1 [] _ = (-1, Vector2D 0 0 V.empty)
stage1 (drawnNumber:ns) boards =
  case filter isWinning boards' of
    (brd:_) -> (drawnNumber, brd)
    _ -> stage1 ns boards'
  where
    boards' = drawNumber drawnNumber <$> boards

stage2 :: [Int] -> [Board] -> (Int, Board)
stage2 (drawnNumber:ns) boards = case partition isWinning boards' of
                            ([lastWinner],[]) -> (drawnNumber, lastWinner)
                            (_, losers) -> stage2 ns losers
  where
    boards' = drawNumber drawnNumber <$> boards


boardScore :: Int -> Board -> Int
boardScore n (Vector2D _ _ v) = n * sum (lefts (V.toList v))

day04 :: IO ()
day04 = do
  inputRaw <- readFile "data/day04.txt"
  let (draws, boards') = parse inputParser inputRaw
      boards :: [Board] = (Left `fmap`) <$> boards'
      (n1, b1) = stage1 draws boards
      (n2, b2) = stage2 draws boards
  print $ boardScore n1 b1
  print $ boardScore n2 b2
  pure ()
