{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import Data.Char (isDigit)
import Data.Functor
import Data.Maybe
import Data.Tuple.Extra
import Text.Regex.Applicative

puzzle1 :: [String] -> Integer
puzzle1 = sum . map (firstAndLast . filter isDigit)

puzzle2 :: [String] -> Integer
puzzle2 = sum . map (\xs -> read [findFirst xs, findLast xs])

firstAndLast :: [Char] -> Integer
firstAndLast xs = read [head xs, last xs]

findFirst :: String -> Char
findFirst = snd3 . fromJust . findFirstInfix textDigit

findLast :: String -> Char
findLast = snd3 . fromJust . findFirstInfix (many anySym *> textDigit)

textDigit :: RE Char Char
textDigit = ("one" <|> "1") $> '1'
         <|> ("two" <|> "2") $> '2'
         <|> ("three" <|> "3") $> '3'
         <|> ("four" <|> "4") $> '4'
         <|> ("five" <|> "5") $> '5'
         <|> ("six" <|> "6") $> '6'
         <|> ("seven" <|> "7") $> '7'
         <|> ("eight" <|> "8") $> '8'
         <|> ("nine" <|> "9") $> '9'
