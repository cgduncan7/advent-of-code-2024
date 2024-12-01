module Day1 (run, part1, part2) where

import Common (Part (P1, P2), Runner, splitStringByFn)
import Data.List (sort)
import Data.Map (Map, empty, lookup, toList)
import Data.Map.Strict (insertWith)
import Debug.Trace (traceShow)

run :: Runner
run P1 [p1Contents] = [part1 p1Contents]
run P2 [p2Contents] = [part2 p2Contents]
run _ [p1Contents, p2Contents] = [part1 p1Contents, part2 p2Contents]

part1 :: String -> Int
part1 input = do
  let separatedLists = map (\str -> map (\word -> read word :: Int) (splitStringByFn str (== ' '))) (lines input)
  let (left, right) = foldl (\acc [l, r] -> (fst acc ++ [l], snd acc ++ [r])) ([], []) separatedLists
  subtractLists (sort left) (sort right)

subtractLists :: [Int] -> [Int] -> Int
subtractLists (h1 : rest1) (h2 : rest2) = abs (h1 - h2) + subtractLists rest1 rest2
subtractLists [] [] = 0

part2 :: String -> Int
part2 input = do
  let separatedLists = map (\str -> map (\word -> read word :: Int) (splitStringByFn str (== ' '))) (lines input)
  let (left, right) = foldl (\(accL, accR) [l, r] -> (accL ++ [l], insertWith (+) r 1 accR)) ([], empty) separatedLists
  foldr (\c acc -> acc + maybe 0 (* c) (Data.Map.lookup c right)) 0 left
