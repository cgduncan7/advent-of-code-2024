module Day4 (run, part1, part2) where

import Common (Bounds, Location, Part (P1, P2), Runner, indexToLocation, locationToIndex)
import Data.Bifunctor (bimap)
import Debug.Trace (trace, traceShow)

run :: Runner
run P1 [p1Contents] = [part1 p1Contents]
run P2 [p2Contents] = [part2 p2Contents]
run _ [p1Contents, p2Contents] = [part1 p1Contents, part2 p2Contents]

part1 :: String -> Int
part1 input = do
  let ls = lines input
  let bounds = (length $ head ls, length ls)
  let cls = concat ls
  sum $ map (\i -> checkForWordAtIndex cls "XMAS" bounds i Nothing) $ take (length cls) [0, 1 ..]

part2 :: String -> Int
part2 input = do
  let ls = lines input
  let bounds = (length $ head ls, length ls)
  let cls = concat ls
  length $ filter (checkIsMASCenter cls bounds) $ take (length cls) [0, 1 ..]

data Dir = UL | U | UR | R | DR | D | DL | L deriving (Show)

nothingIfOutOfBounds :: Bounds -> Location -> Maybe Int
nothingIfOutOfBounds (boundX, boundY) (locX, locY) = do
  if locX < 0 || locY < 0 || locX >= boundX || locY >= boundY then Nothing else Just (locationToIndex (boundX, boundY) (locX, locY))

dirToNeighborIndex :: Bounds -> Dir -> Int -> Maybe Int
dirToNeighborIndex (boundX, boundY) UL idx = nothingIfOutOfBounds (boundX, boundY) (bimap (+ (-1)) (+ (-1)) $ indexToLocation (boundX, boundY) idx)
dirToNeighborIndex (boundX, boundY) U idx = nothingIfOutOfBounds (boundX, boundY) (bimap (+ 0) (+ (-1)) $ indexToLocation (boundX, boundY) idx)
dirToNeighborIndex (boundX, boundY) UR idx = nothingIfOutOfBounds (boundX, boundY) (bimap (+ 1) (+ (-1)) $ indexToLocation (boundX, boundY) idx)
dirToNeighborIndex (boundX, boundY) R idx = nothingIfOutOfBounds (boundX, boundY) (bimap (+ 1) (+ 0) $ indexToLocation (boundX, boundY) idx)
dirToNeighborIndex (boundX, boundY) DR idx = nothingIfOutOfBounds (boundX, boundY) (bimap (+ 1) (+ 1) $ indexToLocation (boundX, boundY) idx)
dirToNeighborIndex (boundX, boundY) D idx = nothingIfOutOfBounds (boundX, boundY) (bimap (+ 0) (+ 1) $ indexToLocation (boundX, boundY) idx)
dirToNeighborIndex (boundX, boundY) DL idx = nothingIfOutOfBounds (boundX, boundY) (bimap (+ (-1)) (+ 1) $ indexToLocation (boundX, boundY) idx)
dirToNeighborIndex (boundX, boundY) L idx = nothingIfOutOfBounds (boundX, boundY) (bimap (+ (-1)) (+ 0) $ indexToLocation (boundX, boundY) idx)

dirsToSearch = [UL, U, UR, R, DR, D, DL, L]

checkForWordAtIndex :: String -> String -> Bounds -> Int -> Maybe Dir -> Int
checkForWordAtIndex text (fc : rc) (bX, bY) idx Nothing =
  if text !! idx == fc
    then
      sum $
        [ case ni of
            Just ni' -> checkForWordAtIndex text rc (bX, bY) ni' (Just nd)
            Nothing -> 0
          | (nd, ni) <- map (\nd -> (nd, dirToNeighborIndex (bX, bY) nd idx)) dirsToSearch
        ]
    else
      0
checkForWordAtIndex text (fc : rc) (bX, bY) idx (Just d) =
  if text !! idx == fc
    then
      if null rc
        then 1
        else case dirToNeighborIndex (bX, bY) d idx of
          Nothing -> 0
          Just ni' -> checkForWordAtIndex text rc (bX, bY) ni' (Just d)
    else
      0
checkForWordAtIndex text [] _ _ _ = 1

checkIsMASCenter :: String -> Bounds -> Int -> Bool
checkIsMASCenter text (bX, bY) idx =
  (text !! idx == 'A')
    && ( case map (map (\nd -> dirToNeighborIndex (bX, bY) nd idx)) [[UL, DR], [UR, DL]] of
           [[Just a, Just b], [Just c, Just d]] -> map (text !!) [a, b] `elem` ["MS", "SM"] && map (text !!) [c, d] `elem` ["MS", "SM"]
           _ -> False
       )
