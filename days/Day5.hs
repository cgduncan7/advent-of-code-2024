module Day5 (run, part1, part2) where

import Common (Part (P1, P2), Runner, splitStringByFn)
import Data.Map (Map, empty, insert, lookup, update)
import Data.Maybe (isNothing)
import Debug.Trace (traceShow)
import Prelude hiding (lookup)

run :: Runner
run P1 [p1Contents] = [part1 p1Contents]
run P2 [p2Contents] = [part2 p2Contents]
run _ [p1Contents, p2Contents] = [part1 p1Contents, part2 p2Contents]

part1 :: String -> Int
part1 input = do
  let ls = lines input
  fst $ calculateFinalAnswer (handleLines (True, empty :: Map Int Node, [], []) ls)

part2 :: String -> Int
part2 input = do
  let ls = lines input
  snd $ calculateFinalAnswer (handleLines (True, empty :: Map Int Node, [], []) ls)

type State = (Bool, Map Int Node, [[Int]], [[Int]])

-- (left, right)
type Node = ([Int], [Int])

handlePair :: Map Int Node -> Int -> Int -> Map Int Node
handlePair mp left right = do
  let lmap = if isNothing $ lookup left mp then insert left ([], [right]) mp else update (\(l, r) -> Just (l, r ++ [right])) left mp
  if isNothing $ lookup right lmap then insert right ([left], []) lmap else update (\(l, r) -> Just (l ++ [left], r)) right lmap

data Dir = L | R

checkDirForVal :: Map Int Node -> Int -> Dir -> Int -> Bool
checkDirForVal mp root R val = do
  -- root right of val
  case lookup root mp of
    Nothing -> False
    Just ([], _) -> False
    Just (left, _) -> val `elem` left
checkDirForVal mp root L val = do
  -- root left of val
  case lookup root mp of
    Nothing -> False
    Just (_, []) -> False
    Just (_, right) -> val `elem` right

handleValues :: Map Int Node -> [Int] -> Bool
handleValues mp (h : rest) = all (checkDirForVal mp h L) rest && handleValues mp rest
handleValues mp [] = True

handleLines :: State -> [String] -> State
handleLines (True, mp, successes, failures) ("" : rest) = handleLines (False, mp, successes, failures) rest -- switching to analyze mode
handleLines (True, mp, successes, failures) (h : rest) = do
  let [l, r] = map (\s -> read s :: Int) $ splitStringByFn h (== '|')
  handleLines (True, handlePair mp l r, successes, failures) rest
handleLines (False, mp, successes, failures) (h : rest) = do
  let vals = map (\s -> read s :: Int) $ splitStringByFn h (== ',')
  (if handleValues mp vals then handleLines (False, mp, successes ++ [vals], failures) rest else handleLines (False, mp, successes, failures ++ [vals]) rest)
handleLines (False, mp, successes, failures) [] = (False, mp, successes, failures)

getMiddleOfList :: [a] -> a
getMiddleOfList list = list !! (length list `div` 2)

fixFailures :: Map Int Node -> [Int] -> [Int]
fixFailures mp (h : rest) = do
  let belongsBefore = filter (checkDirForVal mp h R) rest
  let belongsAfter = filter (checkDirForVal mp h L) rest
  if null belongsBefore then h : fixFailures mp rest else fixFailures mp (belongsBefore ++ [h] ++ belongsAfter)
fixFailures mp [] = []

fixAllFailures :: Map Int Node -> [[Int]] -> [[Int]]
fixAllFailures mp = map (fixFailures mp)

calculateFinalAnswer :: State -> (Int, Int)
calculateFinalAnswer (_, mp, s : srest, failures) = do
  let fixedFailures = fixAllFailures mp failures
  (getMiddleOfList s + calculateFinalAnswer' srest, calculateFinalAnswer' fixedFailures)
calculateFinalAnswer (_, _, [], _) = (0, 0)

calculateFinalAnswer' :: [[Int]] -> Int
calculateFinalAnswer' = foldr ((+) . getMiddleOfList) 0