{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-redundant-bang-patterns #-}
module Adventlib
  ( -- * Functions to work with standard input
    runOnLines
  , runOnFile
    -- * Parsing functions and helpers
  , sepBy
  , space
  , base10
    -- * Utilities
  , countWhile
  , countUntil
  )
where

import Control.Applicative
import Data.Char
import Text.Regex.Applicative
import Data.Foldable

-- | 'runOnLines' runs the provided function on the standard input, split by
-- lines, and prints the result.
runOnLines :: (Show r) => ([String] -> r) -> IO ()
runOnLines f = getContents >>= print . f . lines

-- | 'runOnFile' runs the provided function on the standard input and print the
-- result.
runOnFile :: (Show r) => (String -> r) -> IO ()
runOnFile f = getContents >>= print . f

sepBy :: (Alternative p) => p a -> p sep -> p [a]
sepBy p sep = liftA2 (:) p (many (sep *> p))

space :: RE Char String
space = many $ psym isSpace

base10 :: (Num a, Foldable t) => t a -> a
base10 = foldl' (\d i -> d * 10 + i) 0

countWhile :: (a -> Bool) -> [a] -> Integer
countWhile = countWhileInner 0

countWhileInner :: Integer -> (a -> Bool) -> [a] -> Integer
countWhileInner !n _ [] = n
countWhileInner !n p (x:xs)
  | p x = countWhileInner (n + 1) p xs
  | otherwise = n

countUntil :: (a -> Bool) -> (a -> a) -> a -> Integer
countUntil = countUntilInner 0

countUntilInner :: Integer -> (a -> Bool) -> (a -> a) -> a -> Integer
countUntilInner !n p f x
  | p x = n
  | otherwise = countUntilInner (n + 1) p f (f x)
