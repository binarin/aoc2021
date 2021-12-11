module Day06 where

import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad (forM_)

reproductionRound :: V.Vector Int -> V.Vector Int
reproductionRound = V.modify $ \v -> do
  reproduceCount <- MV.read v 0
  forM_ [1..8] $ \idx -> do
    cur <- MV.read v idx
    MV.write v (idx - 1) cur
  MV.write v 8 reproduceCount
  MV.modify v (+ reproduceCount) 6

initialCount :: [Int] -> V.Vector Int
initialCount ages = V.modify doIt (V.replicate 9 0)
  where
    doIt v = do
      forM_ ages $ \age -> MV.modify v (+1) age


day06 :: IO ()
day06 = do
  inputRaw <- parse (L.decimal `sepBy` char ',' :: Parser [Int]) <$> readFile "data/day06.txt"
  let input = initialCount inputRaw
  print $ V.sum $ head $ drop 80 $ iterate reproductionRound input
  print $ V.sum $ head $ drop 256 $ iterate reproductionRound input
  pure ()
