{-# LANGUAGE LambdaCase #-}
module Day03 where

import Data.Char (ord)

sample :: [String]
sample = ["00100"
         ,"11110"
         ,"10110"
         ,"10111"
         ,"10101"
         ,"01111"
         ,"00111"
         ,"11100"
         ,"10000"
         ,"11001"
         ,"00010"
         ,"01010"
         ]

parse :: [String] -> [[Int]]
parse = fmap (fmap decodeBin)
  where
    decodeBin '0' = 0
    decodeBin '1' = 1

readBinary :: String -> Int
readBinary = foldl (\acc digit -> acc * 2 + (ord digit - ord '0')) 0

main :: IO ()
main = do
  -- report <- pure sample
  report <- lines <$>  readFile "data/day03.txt"
  let parsed = parse report
      freq = foldr (zipWith (+)) (head parsed) (tail parsed)
      threshold = length parsed `div` 2
      gamma = flip map freq $ \case
        n | n > threshold -> '1'
        otherwise -> '0'
      epsilon = flip map freq $ \case
        n | n > threshold -> '0'
        otherwise -> '1'

      bitCriteriaRound pickChar numbers = [ (cs, num) | ((c:cs), num) <- numbers, c == firstChar ]
        where firstChar = curry pickChar zerosCount onesCount
              onesCount = length $ filter (\((c:_), _) -> c == '1') numbers
              zerosCount = length $ filter (\((c:_), _) -> c == '0') numbers

      applyBitCriteria criteria report = snd $ head $ head $ dropWhile (\ns -> length ns /= 1) candidates
        where
          report' = (\n -> (n, n)) <$> report
          candidates = iterate (bitCriteriaRound criteria) report'

      oxygenRating = applyBitCriteria (\case
                                          (zs, os) | zs <= os -> '1' -- Ones are most common
                                          _ -> '0') report
      co2Rating = applyBitCriteria (\case
                                          (zs, os) | zs > os -> '1' -- Ones are least common
                                          _ -> '0') report

  putStrLn $ show (readBinary gamma * readBinary epsilon)
  putStrLn $ show $ readBinary oxygenRating * readBinary co2Rating
  pure ()
