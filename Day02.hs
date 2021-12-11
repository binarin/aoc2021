module Day02 where

import qualified Text.Megaparsec.Char.Lexer as L

data Cmd = Up Int | Down Int | Forward Int deriving (Show)

inputParser :: Parser [Cmd]
inputParser = some (cmd <* newline)
  where
    cmd = choice [ Up <$> (string "up " *> L.decimal)
                 , Down <$> (string "down " *> L.decimal)
                 , Forward <$> (string "forward " *> L.decimal)
                 ]
type Depth = Int
type Position = Int
type Aim = Int

step :: (Depth, Position) -> Cmd -> (Depth, Position)
step (d, p) (Up dd) = (d - dd, p)
step (d, p) (Down dd) = (d + dd, p)
step (d, p) (Forward dp) = (d, p + dp)

stepAim :: (Depth, Position, Aim) -> Cmd -> (Depth, Position, Aim)
stepAim (d, p, a) (Up da) = (d, p, a - da)
stepAim (d, p, a) (Down da) = (d, p, a + da)
stepAim (d, p, a) (Forward x) = (d + a * x, p + x, a)


day02 :: IO ()
day02 = do
  input <- parse inputParser <$> readFile "data/day02.txt"
  let (finalDepth, finalPos) = foldl step (0, 0) input
      (finalDepth2, finalPos2, _) = foldl stepAim (0, 0, 0) input
  print $ finalPos * finalDepth
  print $ finalDepth2 * finalPos2
  pure ()
