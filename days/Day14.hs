module Day14 (run, part1, part2) where

import Common (Bounds, Location, Part (P1, P2), Runner, splitStringByFn)
import Debug.Trace (traceShow)
import Text.Printf (formatInt)

run :: Runner
run P1 [p1Contents] = [part1 p1Contents]
run P2 [p2Contents] = [part2 p2Contents]
run _ [p1Contents, p2Contents] = [part1 p1Contents, part2 p2Contents]

part1 :: String -> Int
part1 input = do
  let l = lines input
  let robots = map handleLine l
  let bounds = (101, 103)
  let (a, b, c, d) = getQuadrantScores bounds $ map (\(p, v) -> roboMove bounds p v 100) robots
  a * b * c * d

part2 :: String -> Int
part2 input = do
  let l = lines input
  let robots = map handleLine l
  let bounds = (101, 103)
  let res = map (\(p, v) -> roboMove bounds p v 1) robots
  let x = moveAndCheck bounds robots 1
  x

type Velocity = (Int, Int)

type Steps = Int

handleLine :: String -> (Location, Velocity)
handleLine str = do
  let [px, py, vx, vy] = map (\s -> read s :: Int) $ splitStringByFn str (\c -> c `elem` ['p', '=', ',', ' ', 'v'])
  ((px, py), (vx, vy))

roboMove :: Bounds -> Location -> Velocity -> Steps -> Location
roboMove (bx, by) (lx, ly) (vx, vy) steps = ((lx + vx * steps) `mod` bx, (ly + vy * steps) `mod` by)

getQuadrant :: Bounds -> Location -> Int
getQuadrant (bx, by) (lx, ly)
  | 0 <= lx && lx < bx `div` 2 && 0 <= ly && ly < by `div` 2 = 1
  | bx `div` 2 < lx && lx < bx && 0 <= ly && ly < by `div` 2 = 2
  | 0 <= lx && lx < bx `div` 2 && by `div` 2 < ly && ly < by = 3
  | bx `div` 2 < lx && lx < bx && by `div` 2 < ly && ly < by = 4
  | otherwise = 0

getQuadrantScores :: Bounds -> [Location] -> (Int, Int, Int, Int)
getQuadrantScores (bx, by) = foldr (\(x, y) (a, b, c, d) -> case getQuadrant (bx, by) (x, y) of 1 -> (a + 1, b, c, d); 2 -> (a, b + 1, c, d); 3 -> (a, b, c + 1, d); 4 -> (a, b, c, d + 1); _ -> (a, b, c, d)) (0, 0, 0, 0)

renderLocs :: Bounds -> Location -> [Location] -> String
renderLocs (bx, by) (lx, ly) locs
  | lx == bx && ly < by = '\n' : renderLocs (bx, by) (0, ly + 1) locs
  | lx == bx && ly == by = ""
  | otherwise = (if (lx, ly) `elem` locs then "X" else " ") ++ renderLocs (bx, by) (lx + 1, ly) locs

moveAndCheck :: Bounds -> [(Location, Velocity)] -> Int -> Int
moveAndCheck bounds robots step =
  do
    let newLocs = map (\(loc, vel) -> roboMove bounds loc vel step) robots
    let possibleMatch =
          any
            ( \(x, y) ->
                (x + 1, y) `elem` newLocs
                  && (x - 1, y) `elem` newLocs
                  && (x, y + 1) `elem` newLocs
                  && (x, y - 1) `elem` newLocs
                  && (x - 1, y - 1) `elem` newLocs
                  && (x - 1, y + 1) `elem` newLocs
                  && (x + 1, y - 1) `elem` newLocs
                  && (x + 1, y + 1) `elem` newLocs
            )
            newLocs
    if possibleMatch
      then
        traceShow (step, '\n', renderLocs bounds (0, 0) newLocs) moveAndCheck bounds robots (step + 1)
      else moveAndCheck bounds robots (step + 1)