module Day10 (run, part1, part2) where

import Common (Bounds, Dir (D, L, R, U), Location, Part (P1, P2), Runner, filterMaybes, move)
import Data.Map qualified (Map, empty, filter, filterWithKey, fold, foldr, foldr', foldrWithKey, insert, lookup, union)
import Data.Set qualified
import Debug.Trace (traceShow)

run :: Runner
run P1 [p1Contents] = [part1 p1Contents]
run P2 [p2Contents] = [part2 p2Contents]
run _ [p1Contents, p2Contents] = [part1 p1Contents, part2 p2Contents]

part1 :: String -> Int
part1 input = do
  let (mp, bounds) = handleInput input (0, 0) Data.Map.empty
  foldr
    ( \loc acc -> do
        let res = explore mp bounds Data.Map.empty loc
        acc + Data.Set.size (snd res)
    )
    0
    (findTrailheads mp)

part2 :: String -> Int
part2 input = do
  let (mp, bounds) = handleInput input (0, 0) Data.Map.empty
  foldr
    ( \loc acc -> do
        let res = explore' mp bounds Data.Map.empty loc
        acc + snd res
    )
    0
    (findTrailheads mp)

handleInput :: String -> Location -> Data.Map.Map Location Int -> (Data.Map.Map Location Int, Bounds)
handleInput ['\n'] (x, y) mp = (mp, (x, y + 1))
handleInput ('\n' : rest) (x, y) mp = handleInput rest (0, y + 1) mp
handleInput (ch : rest) (x, y) mp = handleInput rest (x + 1, y) $ Data.Map.insert (x, y) (read [ch] :: Int) mp
handleInput [] (x, y) mp = (mp, (x + 1, y + 1))

findTrailheads :: Data.Map.Map Location Int -> [Location]
findTrailheads = Data.Map.foldrWithKey (\k v acc -> if v == 0 then acc ++ [k] else acc) []

explore :: Data.Map.Map Location Int -> Bounds -> Data.Map.Map Location (Data.Set.Set Location) -> Location -> (Data.Map.Map Location (Data.Set.Set Location), Data.Set.Set Location)
explore mp bounds visited current = do
  let memoizedRes = Data.Map.lookup current visited
  let currentHeight = Data.Map.lookup current mp
  case (memoizedRes, currentHeight) of
    (Just mh, _) -> (visited, mh)
    (Nothing, Nothing) -> (Data.Map.insert current Data.Set.empty visited, Data.Set.empty)
    (Nothing, Just 9) -> (Data.Map.insert current (Data.Set.singleton current) visited, Data.Set.singleton current)
    (Nothing, Just ch) -> do
      let neighborLocs = filterMaybes $ map (move bounds current) [U, R, D, L]
      let validNeighborLocs = filter (\l -> case Data.Map.lookup l mp of Just nh -> nh == ch + 1; Nothing -> False) neighborLocs
      let paths = map (explore mp bounds visited) validNeighborLocs
      foldr (\(pathVisited, pathNum) (accVisited, accNum) -> (Data.Map.union accVisited pathVisited, Data.Set.union accNum pathNum)) (visited, Data.Set.empty) paths

explore' :: Data.Map.Map Location Int -> Bounds -> Data.Map.Map Location Int -> Location -> (Data.Map.Map Location Int, Int)
explore' mp bounds visited current = do
  let memoizedRes = Data.Map.lookup current visited
  let currentHeight = Data.Map.lookup current mp
  case (memoizedRes, currentHeight) of
    (Just mh, _) -> (visited, mh)
    (Nothing, Nothing) -> (Data.Map.insert current 0 visited, 0)
    (Nothing, Just 9) -> (Data.Map.insert current 1 visited, 1)
    (Nothing, Just ch) -> do
      let neighborLocs = filterMaybes $ map (move bounds current) [U, R, D, L]
      let validNeighborLocs = filter (\l -> case Data.Map.lookup l mp of Just nh -> nh == ch + 1; Nothing -> False) neighborLocs
      let paths = map (explore' mp bounds visited) validNeighborLocs
      foldr (\(pathVisited, pathNum) (accVisited, accNum) -> (Data.Map.union accVisited pathVisited, accNum + pathNum)) (visited, 0) paths