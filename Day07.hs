module Day07 where

import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad (forM_)


vectorizeInput :: [Int] -> Vector Int
vectorizeInput positions = V.modify updateCount (V.replicate (maximum positions + 1) 0)
  where
    updateCount v = forM_ positions $ \pos -> do
      MV.modify v (+1) pos

-- allCosts :: [Int] -> Vector Int
allCosts input = V.fromList $ go (tail $ V.toList positionCount) (sum input) (positionCount V.! 0, length input - positionCount V.! 0)
  where
    lastIndex = maximum input
    positionCount = vectorizeInput input
    go [] _ _ = []
    go (cnt:counts) cost (l, r) = cost:go counts (cost + l - r) (l + cnt, r - cnt)

allCosts' input = zipWith (+) forward backward
  where
    move (cost, total) count = (cost + total, total + count)
    forward = fmap fst $ tail $ scanl move (0, 0) (V.toList $ vectorizeInput input)
    backward = fmap fst $ reverse $ tail $ scanl move (0, 0) (reverse $ V.toList $ vectorizeInput input)

allCosts2' input = zipWith (+) forward backward
  where
    move (cost, step, total) count = (cost + step, step + total + count, total + count)
    forward = fmap fst3 $ tail $ scanl move (0, 0, 0) (V.toList $ vectorizeInput input)
    backward = fmap fst3 $ reverse $ tail $ scanl move (0, 0, 0) (reverse $ V.toList $ vectorizeInput input)
    fst3 (a, _, _) = a


day07 :: IO ()
day07 = do
  input <- parse (L.decimal `sepBy` char ',' :: Parser [Int]) <$> readFile "data/day07.txt"
  -- print $ vectorizeInput input
  print $ V.minimum $ allCosts input
  print $ minimum $ allCosts' input
  print $ minimum $ allCosts2' input
  pure ()
