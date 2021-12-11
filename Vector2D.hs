module Vector2D ( Vector2D(..)
                , at
                ) where

import qualified Data.Vector as V

data Vector2D a = Vector2D Int Int (Vector a)

instance Functor Vector2D where
  fmap :: (a -> b) -> Vector2D a -> Vector2D b
  fmap f (Vector2D rows columns v) = Vector2D rows columns (f <$> v)

instance Foldable Vector2D where
  foldr f b (Vector2D rows columns v) = foldr f b v

at :: Vector2D a -> (Int, Int) -> a
at (Vector2D rows columns v) (y, x) = v V.! (columns * y + x)

instance Show a => Show (Vector2D a) where
  show v@(Vector2D rows columns _) = header ++ mconcat [ showLine y | y <- [0..rows-1] ]
    where
      header = "V2D("++show rows++"x"++show columns++"):\n"
      v' = show <$> v
      maxWidth = maximum (length <$> v' )

      padLeft :: String -> String
      padLeft s = replicate (maxWidth - length s) ' ' ++ s

      showLine :: Int -> String
      showLine y = mconcat $ [ padLeft (v' `at` (y, x)) ++ " " | x <- [0..columns-1] ] ++ ["\n"]
