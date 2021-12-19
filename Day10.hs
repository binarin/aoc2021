module Day10 where

import Data.Void
import Control.Monad (forM_)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.List (sort)
import Data.Maybe
import qualified System.IO

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

isOpen '{' = True
isOpen '(' = True
isOpen '<' = True
isOpen '[' = True
isOpen _ = False

isClose '}' = True
isClose ')' = True
isClose '>' = True
isClose ']' = True
isClose _ = False

isPair '{' '}' = True
isPair '[' ']' = True
isPair '<' '>' = True
isPair '(' ')' = True
isPair _ _ = False


completionCharScore '(' = 1
completionCharScore '[' = 2
completionCharScore '{' = 3
completionCharScore '<' = 4


completionScore :: String -> Int
completionScore s = foldr (\c score -> score * 5 + completionCharScore c) 0 (reverse s)

completingParser :: String -> Maybe String
completingParser = go []
  where
    go stack [] = Just stack
    go stack (c:cs)
      | isOpen c = go (c:stack) cs
    go (top:rest) (c:cs)
      | isClose c && isPair top c = go rest cs
      | otherwise = Nothing

day10 :: IO ()
day10 = do
  let filename = "data/day10.txt"
  input <- parse (some lineParser) <$> readFile filename
  forM_ input print
  print $ sum $ chunkErrorScore <$> input

  input' <- System.IO.readFile filename
  let incomplete = catMaybes $ completingParser <$> lines input'
  print $ incomplete
  print $ (sort $ completionScore <$> incomplete) !! (length incomplete `div` 2)
  pure ()
