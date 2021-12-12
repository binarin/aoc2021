module Day08 where

import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

lineParser :: Parser ([String], [String])
lineParser = (,) <$> (some indicator <* string "| ") <*> (some indicator <* newline)
  where
    indicator = some (oneOf ['a'..'g']) <* optional hspace

inputParser :: Parser [([String], [String])]
inputParser = some lineParser

-- solve :: [String] -> [String] -> [Int]
solve inputs outputs =  foldl (\acc dig -> acc * 10 + dig) 0 decoded --  (decodeMap, S.fromList <$> outputs)
  where
    iCount = M.toList $ foldl (\m c -> M.insertWith (+) c 1 m) M.empty $ mconcat inputs
    [b] = [ x | (x, cnt) <- iCount, cnt == 6]
    [e] = [ x | (x, cnt) <- iCount, cnt == 4]
    [f] = [ x | (x, cnt) <- iCount, cnt == 9]
    [d1s] = filter (\i -> 2 == length i) inputs
    [c] = d1s \\ [f]
    [a] = [ x | (x, cnt) <- iCount, cnt == 8, x /= c ]
    [d4s] = filter (\i -> 4 == length i) inputs
    [d] = d4s \\ [b, c, f]
    [g] = [ x | (x, cnt) <- iCount, cnt == 7, x /= d ]
    decodeMap = M.fromList [ (S.fromList [a,b,c,e,f,g], 0)
                           , (S.fromList [c,f], 1)
                           , (S.fromList [a,c,d,e,g], 2)
                           , (S.fromList [a,c,d,f,g], 3)
                           , (S.fromList [b,c,d,f], 4)
                           , (S.fromList [a,b,d,f,g], 5)
                           , (S.fromList [a,b,d,e,f,g], 6)
                           , (S.fromList [a,c,f], 7)
                           , (S.fromList [a,b,c,d,e,f,g], 8)
                           , (S.fromList [a,b,c,d,f,g], 9)
                           ]
    decoded = (decodeMap M.!) . S.fromList <$> outputs

day08 :: IO ()
day08 = do
  input <- parse inputParser <$> readFile "data/day08.txt"
  let allOutputs = mconcat $ snd <$> input
      easyCount = length $ filter (\l -> l == 2 || l == 3 || l == 4 || l == 7) $ length <$> allOutputs
  print $ easyCount
  print $ solve (fst $ input !! 1) (snd $ input !! 1)
  print $ sum $ uncurry solve <$> input
  pure ()
