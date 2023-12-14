{-# LANGUAGE OverloadedStrings #-}
module Day8Spec where

import Test.Hspec
import Day8
import Text.Regex.Applicative

spec :: Spec
spec = do
  describe "the parsing" $ do
    it "parses the instruction line" $ do
      match parseInstructionLine "LLR" `shouldBe` Just [GoLeft, GoLeft, GoRight]
    it "parses a graph node" $ do
      match parseNode "AAA = (BBB, BBB)" `shouldBe` Just ("AAA", NodePaths "BBB" "BBB")
    it "parses several graph nodes" $ do
      match parseNodes "AAA = (BBB, BBB)\nBBB = (AAA, ZZZ)" `shouldBe` Just [("AAA", NodePaths "BBB" "BBB"), ("BBB", NodePaths "AAA" "ZZZ")]
  describe "the running" $ do
    it "puzzle1 the handout" $ do
      puzzle1 "RL\n\nAAA = (BBB, CCC)\nBBB = (DDD, EEE)\nCCC = (ZZZ, GGG)\nDDD = (DDD, DDD)\nEEE = (EEE, EEE)\nGGG = (GGG, GGG)\nZZZ = (ZZZ, ZZZ)\n"
      `shouldBe`
      2
    it "puzzle2 the handout" $ do
      puzzle2 "LR\n\n11A = (11B, XXX)\n11B = (XXX, 11Z)\n11Z = (11B, XXX)\n22A = (22B, XXX)\n22B = (22C, 22C)\n22C = (22Z, 22Z)\n22Z = (22B, 22B)\nXXX = (XXX, XXX)\n"
      `shouldBe`
      6
