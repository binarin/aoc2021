module Day05 where

import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Vector as V
import Vector2D

-- data Line = Vertical Int Int Int | Horizontal Int Int Int | Slanted (Int, Int) (Int, Int) deriving (Show)

type Coord = (Int, Int)
type Line = (Coord, Coord)

lineParser :: Parser Line
lineParser = (,) <$> (coord <* string " -> ") <*> coord
  where
    coord = (,) <$> (L.decimal <* string ",") <*> L.decimal

bounds :: [Line] -> (Coord, Coord)
bounds = foldr1 expandBounds
  where
    expandBounds ((l1x1, l1y1), (l1x2, l1y2)) ((l2x1, l2y1), (l2x2, l2y2)) = ((minimum [l1x1, l1x2, l2x1, l2x2], minimum [l1y1, l1y2, l2y1, l2y2]),
                                                                              (maximum [l1x1, l1x2, l2x1, l2x2], maximum [l1y1, l1y2, l2y1, l2y2]))

addLine :: Vector2D Int -> Line -> Vector2D Int
addLine (Vector2D rows cols vec) (p1, p2) = Vector2D rows cols (vec V.// updates p1 p2)
  where
    updates (x1, y1) (x2, y2)
      | x1 == x2 = [ (y * cols + x1, 1 + vec V.! (y * cols + x1)) | y <- [min y1 y2..max y1 y2] ]
      | y1 == y2 = [ (y1 * cols + x, 1 + vec V.! (y1 * cols + x)) | x <- [min x1 x2..max x1 x2] ]
      | otherwise = []

addLine' :: Vector2D Int -> Line -> Vector2D Int
addLine' (Vector2D rows cols vec) (p1, p2) = Vector2D rows cols (vec V.// updates p1 p2)
  where
    updates (x1, y1) (x2, y2)
      | x1 == x2 = [ (y * cols + x1, 1 + vec V.! (y * cols + x1)) | y <- [min y1 y2..max y1 y2] ]
      | y1 == y2 = [ (y1 * cols + x, 1 + vec V.! (y1 * cols + x)) | x <- [min x1 x2..max x1 x2] ]
      | otherwise = zipWith (\x y -> (y * cols + x, 1 + vec V.! (y * cols + x))) (range x1 x2) (range y1 y2)
    range c1 c2
      | c1 == c2 = repeat c1
      | c1 < c2 = [c1,c1+1..c2]
      | otherwise = [c1,c1-1..c2]

day05 :: IO ()
day05 = do
  input <- parse (some $ lineParser <* newline) <$> readFile "data/day05.txt"
  let (_, (maxX, maxY)) = bounds input
      diagram = Vector2D (maxY + 1) (maxX + 1) $ V.replicate ((maxY + 1) * (maxX + 1)) 0
      Vector2D _ _ phase1 = foldl addLine diagram input
      Vector2D _ _ phase2 = foldl addLine' diagram input
  print $ length $ filter (> 1) $ V.toList phase1
  print $ length $ filter (> 1) $ V.toList phase2
  pure ()
