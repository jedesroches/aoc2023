module Adventlib
  ( runOnLines,
    sepBy,
    space,
    base10,
  )
where

import Control.Applicative
import Data.Char
import Text.Regex.Applicative
import Data.Foldable

runOnLines :: (Show r) => ([String] -> r) -> IO ()
runOnLines f = getContents >>= print . f . lines

sepBy :: (Alternative p) => p a -> p sep -> p [a]
sepBy p sep = liftA2 (:) p (many (sep *> p))

space :: RE Char String
space = many $ psym isSpace

base10 :: (Num a, Foldable t) => t a -> a
base10 = foldl' (\d i -> d * 10 + i) 0
