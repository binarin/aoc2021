module Day01 where

import qualified Text.Megaparsec.Char.Lexer as L

day01 :: IO ()
day01 = do
  inputRaw <- readFile "data/day01.txt"
  let input = parse (L.decimal `sepEndBy` newline :: Parser [Int]) inputRaw
      increasedP = zipWith (<) input (tail input)
      windows = zipWith3 (\a b c -> a + b + c) input (tail input) (tail $ tail input)
      windowIncreasedP = zipWith (<) windows (tail windows)
  print $ length $ filter id increasedP
  print $ length $ filter id windowIncreasedP
  pure ()
