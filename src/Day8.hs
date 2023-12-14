{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-redundant-bang-patterns #-}

module Day8 where

import Adventlib
import Data.Char
import Data.Functor
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.String
import Text.Regex.Applicative
import Protolude
import Data.List (last)

puzzle1 :: String -> Integer
puzzle1 = uncurry count . fromJust . match parseDesertMap

puzzle2 :: String -> Integer
puzzle2 = uncurry countGhost . fromJust . match parseDesertMap

newtype NodeName = NodeName String
  deriving (Eq, Ord, Show, IsString, NFData)

data NodePaths = NodePaths {left :: !NodeName, right :: !NodeName}
  deriving (Eq, Show)

data Instr = GoLeft | GoRight
  deriving (Eq, Show)

type Graph = Map NodeName NodePaths

startNode :: NodeName
startNode = "AAA"

startCondition :: NodeName -> Bool
startCondition (NodeName n) = last n == 'A'

endNode :: NodeName
endNode = "ZZZ"

endCondition :: NodeName -> Bool
endCondition (NodeName n) = last n == 'Z'

follow :: Graph -> NodeName -> Instr -> NodeName
follow graph start GoLeft = left $ graph ! start
follow graph start GoRight = right $ graph ! start

followGhost :: Graph -> [NodeName] -> Instr -> [NodeName]
followGhost graph nodes instr = force $ map (flip (follow graph) instr) nodes

count :: Graph -> [Instr] -> Integer
count graph =
      countWhile (/= endNode)
    . scanl' (follow graph) startNode
    . cycle

countGhost :: Graph -> [Instr] -> Integer
countGhost graph instr = let infiniteInstr = cycle instr
                             startNodes = filter startCondition $ M.keys graph
                         in stepWhileCounting graph endCondition 0 infiniteInstr startNodes

stepWhileCounting :: Graph -> (NodeName -> Bool) -> Integer -> [Instr] -> [NodeName] -> Integer
stepWhileCounting graph p n (i:is) nodes
  | all p nodes = n
  | otherwise = stepWhileCounting graph p (n + 1) is $ followGhost graph nodes i
stepWhileCounting _ _ n [] _ = n

parseDesertMap :: RE Char (Graph, [Instr])
parseDesertMap =
  flip (,)
    <$> parseInstructionLine
    <* "\n\n"
    <*> (M.fromList <$> parseNodes)
    <* many "\n"

parseInstructionLine :: RE Char [Instr]
parseInstructionLine = many parseInstruction

parseInstruction :: RE Char Instr
parseInstruction = "R" $> GoRight <|> "L" $> GoLeft

parseNodes :: RE Char [(NodeName, NodePaths)]
parseNodes = parseNode `sepBy` "\n"

parseNode :: RE Char (NodeName, NodePaths)
parseNode = (,) <$> parseNodeName <* " = " <*> parseNodePaths

parseNodePaths :: RE Char NodePaths
parseNodePaths =
  NodePaths
    <$> ("(" *> parseNodeName)
    <*> (", " *> parseNodeName <* ")")

parseNodeName :: RE Char NodeName
parseNodeName = NodeName <$> many (psym isAlphaNum)
