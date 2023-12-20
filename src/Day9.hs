{-# LANGUAGE OverloadedStrings #-}

module Day9 where

import Adventlib
import Text.Regex.Applicative
import Text.Regex.Applicative.Common
import Data.Maybe

puzzle1 :: [String] -> Integer
puzzle1 = sum . map (findNextInSequence . fromJust . match parseSequence)

puzzle2 :: [String] -> Integer
puzzle2 = sum . map (findPrevInSequence . fromJust . match parseSequence)

differenceList :: (Integral a) => [a] -> [Integer]
differenceList (x : (y : xs)) = fromIntegral (y - x) : differenceList (y : xs)
differenceList _ = []

findNextInSequence :: [Integer] -> Integer
findNextInSequence = go 0
  where go n xs = let diffList = differenceList xs
                      acc = last xs + n
                  in if all (== 0) diffList
                     then acc
                     else go acc diffList

-- TODO: find a tailrec version
findPrevInSequence :: [Integer] -> Integer
findPrevInSequence xs = let diffList = differenceList xs
                            firstElem = head xs
                        in if all (== 0) diffList
                           then firstElem
                           else firstElem - findPrevInSequence diffList

parseSequence :: RE Char [Integer]
parseSequence = signed decimal `sepBy` " "
