module Day6 (run, part1, part2) where

import Common (Bounds, Dir (D, DL, DR, L, R, U, UL, UR), Location, Part (P1, P2), Runner, move)
import Data.Map (Map, empty, insert, member)
import Data.Set (Set, empty, filter, insert, member, size)
import Debug.Trace (traceShow)
import Prelude hiding (traverse)

run :: Runner
run P1 [p1Contents] = [part1 p1Contents]
run P2 [p2Contents] = [part2 p2Contents]
run _ [p1Contents, p2Contents] = [part1 p1Contents, part2 p2Contents]

part1 :: String -> Int
part1 input = do
  let mp = Data.Map.empty :: Map Location Object
  let locs = Data.Set.empty :: Set Location
  let ls = lines input
  let bounds = (length $ head ls, length ls)
  size $ getLocsFromState (traverse $ handleLines ((Nothing, U), mp, bounds, locs) (0, 0) ls)

part2 :: String -> Int
part2 input = do
  let mp = Data.Map.empty :: Map Location Object
  let locs = Data.Set.empty :: Set (Location, Dir)
  let obs = Data.Set.empty :: Set Location
  let ls = lines input
  let bounds = (length $ head ls, length ls)
  size $ getObsFromState2 $ traverse2 $ handleLines2 ((Nothing, U), mp, bounds, locs, obs) (0, 0) ls

data Object = Obstacle | Guard Dir deriving (Show)

type State = ((Maybe Location, Dir), Map Location Object, Bounds, Set Location)

handleLine :: State -> Location -> String -> State
handleLine state (x, y) ('.' : rest) = handleLine state (x + 1, y) rest
handleLine (g, mp, bounds, locs) (x, y) ('#' : rest) = handleLine (g, Data.Map.insert (x, y) Obstacle mp, bounds, locs) (x + 1, y) rest
handleLine ((loc, dir), mp, bounds, locs) (x, y) ('^' : rest) = handleLine ((Just (x, y), U), mp, bounds, locs) (x + 1, y) rest
handleLine state _ [] = state

handleLines :: State -> Location -> [String] -> State
handleLines state (x, y) (f : rest) = handleLines (handleLine state (x, y) f) (x, y + 1) rest
handleLines state _ [] = state

rotate :: Dir -> Dir
rotate U = R
rotate R = D
rotate D = L
rotate L = U

traverse :: State -> State
traverse ((Just (gx, gy), gd), mp, bounds, locs) = do
  let newLocs = Data.Set.insert (gx, gy) locs
  case move bounds (gx, gy) gd of
    Nothing -> ((Nothing, gd), mp, bounds, newLocs)
    Just ngl -> if Data.Map.member ngl mp then traverse ((Just (gx, gy), rotate gd), mp, bounds, newLocs) else traverse ((Just ngl, gd), mp, bounds, newLocs)
traverse ((Nothing, gd), mp, bounds, locs) = do
  ((Nothing, gd), mp, bounds, locs)

getLocsFromState :: State -> Set Location
getLocsFromState (_, _, _, locs) = locs

-- Current location, map of objects, bounds, set of visited locations+dirs, set of potential obstacles
type State2 = ((Maybe Location, Dir), Map Location Object, Bounds, Set (Location, Dir), Set Location)

handleLine2 :: State2 -> Location -> String -> State2
handleLine2 state (x, y) ('.' : rest) = handleLine2 state (x + 1, y) rest
handleLine2 (g, mp, bounds, locs, obs) (x, y) ('#' : rest) = handleLine2 (g, Data.Map.insert (x, y) Obstacle mp, bounds, locs, obs) (x + 1, y) rest
handleLine2 ((loc, dir), mp, bounds, locs, obs) (x, y) ('^' : rest) = handleLine2 ((Just (x, y), U), mp, bounds, locs, obs) (x + 1, y) rest
handleLine2 state _ [] = state

handleLines2 :: State2 -> Location -> [String] -> State2
handleLines2 state (x, y) (f : rest) = handleLines2 (handleLine2 state (x, y) f) (x, y + 1) rest
handleLines2 state _ [] = state

traverse2 :: State2 -> State2
traverse2 ((Just (gx, gy), gd), mp, bounds, locs, obs) = do
  let newLocs = Data.Set.insert ((gx, gy), gd) locs
  case move bounds (gx, gy) gd of
    Nothing -> ((Nothing, gd), mp, bounds, newLocs, obs)
    Just ngl ->
      if Data.Map.member ngl mp
        then traverse2 ((Just (gx, gy), rotate gd), mp, bounds, newLocs, obs)
        else traverse2 ((Just ngl, gd), mp, bounds, newLocs, if null (Data.Set.filter (\(l, _) -> l == ngl) newLocs) && traverse2' ((Just (gx, gy), rotate gd), Data.Map.insert ngl Obstacle mp, bounds, newLocs, obs) then Data.Set.insert ngl obs else obs)
traverse2 ((Nothing, gd), mp, bounds, locs, obs) = ((Nothing, gd), mp, bounds, locs, obs)

traverse2' :: State2 -> Bool
traverse2' ((Just (gx, gy), gd), mp, bounds, locs, obs) = do
  Data.Set.member ((gx, gy), gd) locs
    || case move bounds (gx, gy) gd of
      Nothing -> False
      Just ngl ->
        if Data.Map.member ngl mp
          then traverse2' ((Just (gx, gy), rotate gd), mp, bounds, Data.Set.insert ((gx, gy), gd) locs, obs)
          else traverse2' ((Just ngl, gd), mp, bounds, Data.Set.insert ((gx, gy), gd) locs, obs)

getObsFromState2 :: State2 -> Set Location
getObsFromState2 (_, _, _, _, obs) = obs