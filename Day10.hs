module Day10 where

import Data.Void
import Control.Monad (forM_)
import Data.List.NonEmpty

data Chunk = Valid | Invalid Char | Incomplete | WTF (ParseError Text Void) deriving (Show)

chunkErrorScore :: Chunk -> Integer
chunkErrorScore (Invalid ')') = 3
chunkErrorScore (Invalid ']') = 57
chunkErrorScore (Invalid '}') = 1197
chunkErrorScore (Invalid '>') = 25137
chunkErrorScore _ = 0

chunkParser :: Parser ()
chunkParser = void $ some nested
  where
    chunks = void $ try $ many chunkParser
    nested = string "{" *> chunks <* string "}"
         <|> string "[" *> chunks <* string "]"
         <|> string "(" *> chunks <* string ")"
         <|> string "<" *> chunks <* string ">"

lineParser :: Parser Chunk
lineParser = do
  r <- observing chunkParser >>= \case
    Left (TrivialError _ (Just (Tokens (c :| _))) _) -> case c of
                                                          '\n' -> pure Incomplete
                                                          _ -> pure $ Invalid c
    Left e -> do
      pure $ WTF e
    Right _ -> do
      pure $ Valid
  many $ satisfy (/= '\n')
  newline
  pure r

day10 :: IO ()
day10 = do
  input <- parse (some lineParser) <$> readFile "data/day10-sample.txt"
  forM_ input print
  print $ sum $ chunkErrorScore <$> input
  pure ()
