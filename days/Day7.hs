module Day7 (run, part1, part2) where

import Common (Part (P1, P2), Runner, splitStringByFn)
import Debug.Trace (traceShow)

run :: Runner
run P1 [p1Contents] = [part1 p1Contents]
run P2 [p2Contents] = [part2 p2Contents]
run _ [p1Contents, p2Contents] = [part1 p1Contents, part2 p2Contents]

part1 :: String -> Int
part1 input = do
  let ls = lines input
  let eqs = map handleLine ls
  sum $ map fst $ filter (\eq -> checkEquation eq [Add, Mul]) eqs

part2 :: String -> Int
part2 input = do
  let ls = lines input
  let eqs = map handleLine ls
  sum $ map fst $ filter (\eq -> checkEquation eq [Add, Concat, Mul]) eqs

type Equation = (Int, [Int])

data Operator = Add | Concat | Mul

handleLine :: String -> Equation
handleLine s = do
  let nums = map (\s -> read s :: Int) (splitStringByFn s (`elem` ": "))
  (head nums, tail nums)

evaluate :: Int -> Operator -> Int -> Int
evaluate a Add b = a + b
evaluate a Mul b = a * b
evaluate a Concat b = read (show a ++ show b) :: Int

checkEquation :: Equation -> [Operator] -> Bool
checkEquation (total, [f]) _ = total == f
checkEquation (total, [f, s]) allowedOps = any (\op -> total - evaluate f op s == 0) allowedOps
checkEquation (total, f : s : r) allowedOps = any (\op -> checkEquation (total, evaluate f op s : r) allowedOps) allowedOps
