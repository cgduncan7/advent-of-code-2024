{-# LANGUAGE LambdaCase #-}

module Day12 (run, part1, part2) where

import Common (Dir (D, L, R, U), Location, Part (P1, P2), Runner, move')
import Data.IntMap (update)
import Data.List (partition)
import Data.Map qualified
import Data.Set qualified
import Debug.Trace (traceShow)
import GHC.Float (int2Float)

run :: Runner
run P1 [p1Contents] = [part1 p1Contents]
run P2 [p2Contents] = [part2 p2Contents]
run _ [p1Contents, p2Contents] = [part1 p1Contents, part2 p2Contents]

part1 :: String -> Int
part1 input =
  sum $ Data.Map.map getCost (handleChar input (0, 0) Data.Map.empty)

part2 :: String -> Int
part2 input = do
  let res = handleChar input (0, 0) Data.Map.empty
  sum $ Data.Map.map getCost' res

type Area = Int

type Perimeter = Int

type VertexDegree = Data.Map.Map (Float, Float) [(Float, Float)]

type Crop = ([Location], Area, Perimeter, VertexDegree)

findPresentNeighbors :: Location -> [Location] -> [Location]
findPresentNeighbors loc locs = do
  let possibleNeighbors = map (move' loc) [U, L, R, D]
  filter (`elem` possibleNeighbors) locs

getVertices :: Location -> [(Float, Float, [(Float, Float)])]
getVertices (x, y) = do
  let (fx, fy) = (int2Float x, int2Float y)
  [ (fx - 0.5, fy - 0.5, [(fx - 0.5, fy + 0.5), (fx + 0.5, fy - 0.5)]),
    (fx + 0.5, fy - 0.5, [(fx + 0.5, fy + 0.5), (fx - 0.5, fy - 0.5)]),
    (fx - 0.5, fy + 0.5, [(fx + 0.5, fy + 0.5), (fx - 0.5, fy - 0.5)]),
    (fx + 0.5, fy + 0.5, [(fx - 0.5, fy + 0.5), (fx + 0.5, fy - 0.5)])
    ]

updateVertices :: Location -> VertexDegree -> VertexDegree
updateVertices loc vd = do
  foldr (\(x, y, edges) acc -> Data.Map.alter (\case Nothing -> Just edges; Just x -> Just $ x ++ edges) (x, y) acc) vd (getVertices loc)

handleChar :: String -> Location -> Data.Map.Map Char [Crop] -> Data.Map.Map Char [Crop]
handleChar [] _ mp = mp
handleChar ['\n'] _ mp = mp
handleChar ('\n' : rest) (x, y) mp = handleChar rest (0, y + 1) mp
handleChar (ch : rest) (x, y) mp = do
  handleChar rest (x + 1, y) $ case Data.Map.lookup ch mp of
    Nothing -> Data.Map.insert ch [([(x, y)], 1, 4, updateVertices (x, y) Data.Map.empty)] mp
    Just crops -> case partition (\(cls, _, _, _) -> not $ null $ findPresentNeighbors (x, y) cls) crops of
      ([], otherCrops) -> Data.Map.update (\_ -> Just (otherCrops ++ [([(x, y)], 1, 4, updateVertices (x, y) Data.Map.empty)])) ch mp
      ([(locs, a, p, vd)], otherCrops) -> Data.Map.update (\_ -> Just $ otherCrops ++ [(locs ++ [(x, y)], a + 1, p + 4 - (2 * length (findPresentNeighbors (x, y) locs)), updateVertices (x, y) vd)]) ch mp
      ([(l1, a1, p1, vd1), (l2, a2, p2, vd2)], otherCrops) -> Data.Map.update (\_ -> Just $ otherCrops ++ [(l1 ++ l2 ++ [(x, y)], a1 + a2 + 1, p1 + p2 + 4 - (2 * length (findPresentNeighbors (x, y) (l1 ++ l2))), Data.Map.unionWith (++) (updateVertices (x, y) vd1) vd2)]) ch mp

getCost :: [Crop] -> Int
getCost crops = sum $ map (\(_, a, p, _) -> a * p) crops

getSides :: VertexDegree -> Int
getSides =
  Data.Map.foldrWithKey
    ( \k v acc -> do
        let f v = zip v (take (length v) [1, 1 ..])
        let mapFromList v = Data.Map.fromListWith (+) (f v)
        if even (Data.Map.size $ mapFromList v) && Data.Map.size (Data.Map.filter (== 1) $ mapFromList v) > 1
          then acc + if Data.Map.size (Data.Map.filter (== 1) $ mapFromList v) == 2 then 1 else 2
          else acc
    )
    0

getCost' :: [Crop] -> Int
getCost' crops = sum $ map (\(loc, a, _, vd) -> a * getSides vd) crops