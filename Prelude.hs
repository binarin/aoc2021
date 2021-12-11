{-# language NoImplicitPrelude #-}

module Prelude
  ( module C
  , module Prelude
  , module Text.Megaparsec
  , module Text.Megaparsec.Char
  , module T
  , module Control.Monad
  , module Data.Either
  , V.Vector
  , readFile
  ) where

import BasePrelude as C hiding (readFile)

import Data.Either (isLeft, isRight, lefts, rights)
import Control.Monad (void)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Data.Void (Void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (readFile)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Debug.Trace (traceShow)

type Parser = Parsec Void Text

parse p input = case Text.Megaparsec.runParser p "" input of
                  Left e -> error $ errorBundlePretty e
                  Right x -> x
