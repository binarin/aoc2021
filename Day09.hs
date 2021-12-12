module Day09 where

import qualified Data.Map as M
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Char as C
import Data.List (sort)

data Plane a = Plane a (M.Map (Int, Int) a)

instance Show a => Show (Plane a) where
  show pl@(Plane _ m) = header ++ mconcat [ showLine y | y <- [tly..bry] ]
    where
      header = "Plane:\n"
      coords = M.keys m
      tlx = minimum $ fst <$> coords
      tly = minimum $ snd <$> coords
      brx = maximum $ fst <$> coords
      bry = maximum $ snd <$> coords
      maxWidth = 1 + (maximum $ length . show <$> M.elems m)
      padLeft s = replicate (maxWidth - length s) ' ' ++ s
      showLine y = mconcat $ [ padLeft (show (planeAt x y pl)) | x <- [tlx..brx] ] ++ ["\n"]


makePlane dflt initial = Plane dflt (M.fromList $ mconcat initial')
  where
    rows = zipWith (,) [0..] initial
    initial' = (\(y, columns) -> (\(x, val) -> ((x, y), val)) <$> zipWith (,) [0..] columns) <$> rows

planeAt :: Int -> Int -> Plane a -> a
planeAt x y (Plane dflt m) = M.findWithDefault dflt (x, y) m

-- lowPoints :: Plane Int -> [Int]
lowPoints pl@(Plane _ cells) = M.filterWithKey isLow cells
  where
    isLow (x, y) val = val < minimum [ planeAt (x-1) y pl
                                     , planeAt (x+1) y pl
                                     , planeAt x (y-1) pl
                                     , planeAt x (y+1) pl
                                     ]

reverseFlood [] pl = pl
reverseFlood ((x, y):pts) pl
  | planeAt x y pl >= 9 = reverseFlood pts pl
  | planeAt x y pl == (-1) = reverseFlood pts pl

reverseFlood q@((x, y):pts) pl@(Plane dflt m) = reverseFlood (pts ++ filter flowsUp adjacent) (Plane dflt m')
  where
    adjacent = [(x-1, y)
               ,(x+1, y)
               ,(x, y-1)
               ,(x, y+1)
               ]
    flowsUp (tx, ty)
      | planeAt tx ty pl >= planeAt x y pl = True
      | planeAt tx ty pl == (-1) = False
      | otherwise = True
    m' = M.insert (x, y) (-1) m

inputParser :: Parser [[Int]]
inputParser = some (some ((\c -> C.ord c - C.ord '0') <$> satisfy C.isDigit) <* newline)

bazinSize :: Plane Int -> Int
bazinSize (Plane _ m) = length $ filter (==(-1)) $ M.elems m

day09 :: IO ()
day09 = do
  input <- makePlane 9 . parse inputParser <$> readFile "data/day09.txt"
  print $ sum $ (+1) . snd <$> (M.toList $ lowPoints input)
  let lowCoords = fst <$> M.toList (lowPoints input)
      bazins = (\c -> reverseFlood [c] input) <$> lowCoords
  print $ product $ take 3 $ reverse $ sort $ bazinSize <$> bazins
  pure ()
