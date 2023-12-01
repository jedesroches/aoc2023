{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day1 where

import Protolude

import Data.Tuple.Extra (snd3)
import Data.String (String)
import Text.Regex.Applicative

puzzle1 :: (Num i, Read i) => [String] -> Maybe i
puzzle1 = fmap sum . mapM (firstAndLastOfList . filter isDigit)

puzzle2 :: (Num i, Read i) => [String] -> Maybe i
puzzle2 = fmap sum . mapM firstAndLastTextDigit

firstAndLastOfList :: (Read i) => String -> Maybe i
firstAndLastOfList xs = sequence [headMay xs, lastMay xs] >>= readMaybe

firstAndLastTextDigit :: (Read i) => String -> Maybe i
firstAndLastTextDigit xs = sequence [findFirst xs, findLast xs] >>= readMaybe

findFirst :: String -> Maybe Char
findFirst = fmap snd3 . findFirstInfix textDigit

findLast :: String -> Maybe Char
findLast = fmap snd3 . findFirstInfix (many anySym *> textDigit)

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
