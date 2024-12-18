module Day13 (run, part1, part2, solve) where

import Common (Part (P1, P2), Runner, filterMaybes, isInteger, splitStringByFn)
import Debug.Trace (traceShow)
import GHC.Float (double2Int)

run :: Runner
run P1 [p1Contents] = [part1 p1Contents]
run P2 [p2Contents] = [part2 p2Contents]
run _ [p1Contents, p2Contents] = [part1 p1Contents, part2 p2Contents]

part1 :: String -> Int
part1 input = do
  let l = lines input
  let eqs = handleLines l
  double2Int $ foldr (\(a, b) acc -> acc + a * 3 + b) 0 $ filterMaybes $ map solve eqs

part2 :: String -> Int
part2 input = do
  let l = lines input
  let eqs = map (\((x, ax, bx), (y, ay, by)) -> ((x + 10000000000000, ax, bx), (y + 10000000000000, ay, by))) (handleLines l)
  double2Int $ foldr (\(a, b) acc -> acc + a * 3 + b) 0 $ filterMaybes $ map solve eqs

getFirst :: (a, a, a) -> a
getFirst (a, _, _) = a

handleLines :: [String] -> [((Double, Double, Double), (Double, Double, Double))]
handleLines [] = []
handleLines [""] = []
handleLines ("" : rest) = handleLines rest
handleLines (l1 : l2 : l3 : rest) = do
  let (ax, ay) = handleLine l1
  let (bx, by) = handleLine l2
  let (x, y) = handleLine l3
  ((x, ax, bx), (y, ay, by)) : handleLines rest

handleLine :: String -> (Double, Double)
handleLine s = do
  let ws = splitStringByFn s (\c -> c `elem` [' ', ','])
  let vals = map (\w -> last $ splitStringByFn w (\c -> c `elem` ['+', '='])) (filter (\w -> head w `elem` ['X', 'Y']) ws)
  case vals of
    [ax, ay] -> (read ax :: Double, read ay :: Double)
    _ -> error "improperly formatted line"

solve :: ((Double, Double, Double), (Double, Double, Double)) -> Maybe (Double, Double)
solve ((x, ax, bx), (y, ay, by)) = do
  let c = ax / ay
  let b = (x * ay - y * ax) / (bx * ay - ax * by)
  let a = (x - bx * b) / ax
  if isInteger a && isInteger b then Just (a, b) else Nothing