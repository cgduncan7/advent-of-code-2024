module Day8 (run, part1, part2) where

import Common (Bounds, Location, Part (P1, P2), Runner, nothingIfLocOutOfBounds, pathTo)
import Data.Map qualified (Map, alter, empty, foldr, foldrWithKey, map, update)
import Data.Maybe
import Data.Set (Set, empty, foldr, fromList, insert, size, union)
import Debug.Trace (traceShow)

run :: Runner
run P1 [p1Contents] = [part1 p1Contents]
run P2 [p2Contents] = [part2 p2Contents]
run _ [p1Contents, p2Contents] = [part1 p1Contents, part2 p2Contents]

part1 :: String -> Int
part1 input = do
  let (mp, (bx, by)) = handleChar input (0, 0) Data.Map.empty
  size $ findAntinodes mp (bx + 1, by + 1) findAntinodes'

part2 :: String -> Int
part2 input = do
  let (mp, (bx, by)) = handleChar input (0, 0) Data.Map.empty
  size $ findAntinodes mp (bx + 1, by + 1) findAntinodes''

upsertLocations :: Maybe [Location] -> Location -> Maybe [Location]
upsertLocations ls (x, y) = case ls of Just l -> Just (l ++ [(x, y)]); Nothing -> Just [(x, y)]

handleChar :: String -> Location -> Data.Map.Map Char [Location] -> (Data.Map.Map Char [Location], Bounds)
handleChar [] (x, y) mp = (mp, (x - 1, y))
handleChar ['\n'] (x, y) mp = (mp, (x - 1, y))
handleChar ('.' : rest) (x, y) mp = handleChar rest (x + 1, y) mp
handleChar ('\n' : rest) (x, y) mp = handleChar rest (0, y + 1) mp
handleChar (ch : rest) (x, y) mp = handleChar rest (x + 1, y) (Data.Map.alter (\ls -> upsertLocations ls (x, y)) ch mp)

findAntinodes :: Data.Map.Map Char [Location] -> Bounds -> ([Location] -> Bounds -> Set Location) -> Set Location
findAntinodes mp bounds fx = Data.Map.foldr (\locs acc -> Data.Set.union (fx locs bounds) acc) Data.Set.empty mp

findAntinodes' :: [Location] -> Bounds -> Set Location
findAntinodes' ((hx, hy) : rest) bounds = do
  Data.Set.union (Prelude.foldr Data.Set.insert Data.Set.empty $ concatMap (mapMaybe (nothingIfLocOutOfBounds bounds) . (\(dx, dy) -> [(hx - dx, hy - dy), (hx + 2 * dx, hy + 2 * dy)]) . pathTo (hx, hy)) rest) (findAntinodes' rest bounds)
findAntinodes' [] _ = Data.Set.empty

findAntinodes'' :: [Location] -> Bounds -> Set Location
findAntinodes'' ((hx, hy) : rest) bounds = do
  let paths = map (pathTo (hx, hy)) rest
  Data.Set.union (Data.Set.fromList $ concatMap (\p -> expandAntinode Pos (hx, hy) bounds p ++ expandAntinode Neg (hx, hy) bounds p) paths ++ [(hx, hy)]) (findAntinodes'' rest bounds)
findAntinodes'' [] _ = Data.Set.empty

data Scale = Pos | Neg

expandAntinode :: Scale -> Location -> Bounds -> (Int, Int) -> [Location]
expandAntinode Pos (x, y) bounds (dx, dy) = do
  case nothingIfLocOutOfBounds bounds (x + dx, y + dy) of
    Just loc -> loc : expandAntinode Pos loc bounds (dx, dy)
    Nothing -> []
expandAntinode Neg (x, y) bounds (dx, dy) = do
  case nothingIfLocOutOfBounds bounds (x - dx, y - dy) of
    Just loc -> loc : expandAntinode Neg loc bounds (dx, dy)
    Nothing -> []