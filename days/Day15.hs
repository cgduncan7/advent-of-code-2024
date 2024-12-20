module Day15 (run, part1, part2) where

import Common (Bounds, Dir (D, L, R, U), Location, Part (P1, P2), Runner, indexToLocation, move', oppositeDir)
import Data.List (sort, sortBy)
import Data.Map qualified
import Data.Set qualified
import Debug.Trace (traceShow)
import Text.Printf (printf)

run :: Runner
run P1 [p1Contents] = [part1 p1Contents]
run P2 [p2Contents] = [part2 p2Contents]
run _ [p1Contents, p2Contents] = [part1 p1Contents, part2 p2Contents]

part1 :: String -> Int
part1 input = do
  let (_, grid) = handleChar input (0, 0) ((0, 0), Data.Map.empty)
  getGPSCoordsSum grid

part2 :: String -> Int
part2 input = do
  let (_, grid) = handleChar' input (0, 0) ((0, 0), Data.Map.empty)
  getGPSCoordsSum' grid

data Object = Wall | Box | Robot deriving (Eq, Show)

type Grid = Data.Map.Map Location Object

type State = (Location, Grid)

setupGrid :: Grid -> Location -> Object -> Grid
setupGrid grid (x, y) obj = Data.Map.insert (x, y) obj grid

canMove :: Grid -> Location -> Dir -> (Bool, Maybe Location)
canMove grid (x, y) d = case Data.Map.lookup (move' (x, y) d) grid of
  Nothing -> (True, Just (x, y))
  Just Wall -> (False, Nothing)
  Just Box -> canMove grid (move' (x, y) d) d
  _ -> (False, Nothing)

updateGrid :: Grid -> Location -> Dir -> (Grid, Maybe Location)
updateGrid grid (x, y) dir =
  case Data.Map.lookup (x, y) grid of
    Just Robot -> (Data.Map.alter (const Nothing) (x, y) (Data.Map.alter (\_ -> Just Robot) (move' (x, y) dir) grid), Just $ move' (x, y) dir)
    Just Box -> updateGrid (Data.Map.alter (const Nothing) (x, y) (Data.Map.alter (\_ -> Just Box) (move' (x, y) dir) grid)) (move' (x, y) (oppositeDir dir)) dir
    _ -> (grid, Nothing)

moveRobot :: Grid -> Location -> Dir -> (Grid, Maybe Location)
moveRobot grid (x, y) dir = do
  let (status, lastLocation) = canMove grid (x, y) dir
  ( if status
      then
        ( case lastLocation of
            Just loc -> updateGrid grid loc dir
            x -> (grid, Nothing)
        )
      else (grid, Nothing)
    )

handleChar :: String -> Location -> State -> State
handleChar [] _ state = state
handleChar ['\n'] _ state = state
handleChar ('.' : rest) (x, y) (roboLoc, grid) = handleChar rest (x + 1, y) (roboLoc, grid)
handleChar ('#' : rest) (x, y) (roboLoc, grid) = handleChar rest (x + 1, y) (roboLoc, setupGrid grid (x, y) Wall)
handleChar ('O' : rest) (x, y) (roboLoc, grid) = handleChar rest (x + 1, y) (roboLoc, setupGrid grid (x, y) Box)
handleChar ('@' : rest) (x, y) (roboLoc, grid) = handleChar rest (x + 1, y) ((x, y), setupGrid grid (x, y) Robot)
handleChar ('\n' : rest) (x, y) (roboLoc, grid) = handleChar rest (0, y + 1) (roboLoc, grid)
handleChar ('<' : rest) (x, y) (roboLoc, grid) = do
  let (newGrid, rl) = moveRobot grid roboLoc L
  case rl of
    Just rl' -> handleChar rest (0, 0) (rl', newGrid)
    _ -> handleChar rest (0, 0) (roboLoc, newGrid)
handleChar ('^' : rest) (x, y) (roboLoc, grid) = do
  let (newGrid, rl) = moveRobot grid roboLoc U
  case rl of
    Just rl' -> handleChar rest (0, 0) (rl', newGrid)
    _ -> handleChar rest (0, 0) (roboLoc, newGrid)
handleChar ('>' : rest) (x, y) (roboLoc, grid) = do
  let (newGrid, rl) = moveRobot grid roboLoc R
  case rl of
    Just rl' -> handleChar rest (0, 0) (rl', newGrid)
    _ -> handleChar rest (0, 0) (roboLoc, newGrid)
handleChar ('v' : rest) (x, y) (roboLoc, grid) = do
  let (newGrid, rl) = moveRobot grid roboLoc D
  case rl of
    Just rl' -> handleChar rest (0, 0) (rl', newGrid)
    _ -> handleChar rest (0, 0) (roboLoc, newGrid)

getGPSCoordsSum :: Grid -> Int
getGPSCoordsSum = Data.Map.foldrWithKey (\(x, y) v acc -> if v == Box then acc + (y * 100 + x) else acc) 0

-- part 2

data Object' = Wall' | Box' Location | Robot' deriving (Eq, Ord, Show)

type Grid' = Data.Map.Map Location Object'

type State' = (Location, Grid')

setupGrid' :: Grid' -> Location -> Object' -> Grid'
setupGrid' grid (x, y) obj = Data.Map.insert (x, y) obj grid

sortMoves' :: (Location, Object') -> (Location, Object') -> Ordering
sortMoves' ((ax, ay), _) ((bx, by), _)
  | ay == by = compare ax bx
  | otherwise = compare ay by

canMove' :: Grid' -> Location -> Dir -> [(Location, Object')] -> (Bool, [(Location, Object')])
canMove' grid (x, y) U locs = case Data.Map.lookup (move' (x, y) U) grid of
  Nothing -> (True, locs)
  Just Wall' -> (False, [])
  Just (Box' (ox, oy)) -> do
    let (ls, ll) = canMove' grid (move' (x, y) U) U [(move' (x, y) U, Box' (ox, oy))]
    let (rs, rl) = canMove' grid (ox, oy) U [((ox, oy), Box' (move' (x, y) U))]
    let sorted = sortBy sortMoves' (locs ++ Data.Set.toList (Data.Set.fromList (ll ++ rl)))
    if ls && rs then (True, reverse sorted) else (False, [])
  _ -> (False, [])
canMove' grid (x, y) D locs = case Data.Map.lookup (move' (x, y) D) grid of
  Nothing -> (True, locs)
  Just Wall' -> (False, [])
  Just (Box' (ox, oy)) -> do
    let (ls, ll) = canMove' grid (move' (x, y) D) D [(move' (x, y) D, Box' (ox, oy))]
    let (rs, rl) = canMove' grid (ox, oy) D [((ox, oy), Box' (move' (x, y) D))]
    let sorted = sortBy sortMoves' (locs ++ Data.Set.toList (Data.Set.fromList (ll ++ rl)))
    if ls && rs then (True, sorted) else (False, [])
  _ -> (False, [])
canMove' grid (x, y) d locs = case Data.Map.lookup (move' (x, y) d) grid of
  Nothing -> (True, locs)
  Just Wall' -> (False, [])
  Just (Box' (ox, oy)) -> canMove' grid (move' (x, y) d) d (locs ++ [(move' (x, y) d, Box' (ox, oy))])
  _ -> (False, [])

updateGrid' :: Grid' -> [(Location, Object')] -> Dir -> (Grid', Maybe Location)
updateGrid' grid [] dir = (grid, Nothing)
updateGrid' grid [(l, Robot')] dir = (Data.Map.alter (const Nothing) l (Data.Map.alter (\_ -> Just Robot') (move' l dir) grid), Just (move' l dir))
updateGrid' grid ((l, o) : rlo) dir =
  updateGrid'
    ( case o of
        Robot' -> Data.Map.alter (const Nothing) l (Data.Map.alter (\_ -> Just Robot') (move' l dir) grid)
        (Box' (bx, by)) -> Data.Map.alter (const Nothing) l (Data.Map.alter (\_ -> Just (Box' (move' (bx, by) dir))) (move' l dir) grid)
        _ -> grid
    )
    rlo
    dir

moveRobot' :: Grid' -> Location -> Dir -> (Grid', Maybe Location)
moveRobot' grid (x, y) dir = do
  let (status, locations) = canMove' grid (x, y) dir [((x, y), Robot')]
  ( if status
      then updateGrid' grid (reverse locations) dir
      else (grid, Nothing)
    )

handleChar' :: String -> Location -> State' -> State'
handleChar' [] _ state = state
handleChar' ['\n'] _ state = state
handleChar' ('.' : rest) (x, y) (roboLoc, grid) = handleChar' rest (x + 2, y) (roboLoc, grid)
handleChar' ('#' : rest) (x, y) (roboLoc, grid) = handleChar' rest (x + 2, y) (roboLoc, setupGrid' (setupGrid' grid (x, y) Wall') (x + 1, y) Wall')
handleChar' ('O' : rest) (x, y) (roboLoc, grid) = handleChar' rest (x + 2, y) (roboLoc, setupGrid' (setupGrid' grid (x + 1, y) (Box' (x, y))) (x, y) (Box' (x + 1, y)))
handleChar' ('@' : rest) (x, y) (roboLoc, grid) = handleChar' rest (x + 2, y) ((x, y), setupGrid' grid (x, y) Robot')
handleChar' ('\n' : rest) (x, y) (roboLoc, grid) = handleChar' rest (0, y + 1) (roboLoc, grid)
handleChar' ('<' : rest) (x, y) (roboLoc, grid) = do
  let (newGrid, rl) = moveRobot' grid roboLoc L
  case rl of
    Just rl' -> handleChar' rest (0, 0) (rl', newGrid)
    _ -> handleChar' rest (0, 0) (roboLoc, newGrid)
handleChar' ('^' : rest) (x, y) (roboLoc, grid) = do
  let (newGrid, rl) = moveRobot' grid roboLoc U
  case rl of
    Just rl' -> handleChar' rest (0, 0) (rl', newGrid)
    _ -> handleChar' rest (0, 0) (roboLoc, newGrid)
handleChar' ('>' : rest) (x, y) (roboLoc, grid) = do
  let (newGrid, rl) = moveRobot' grid roboLoc R
  case rl of
    Just rl' -> handleChar' rest (0, 0) (rl', newGrid)
    _ -> handleChar' rest (0, 0) (roboLoc, newGrid)
handleChar' ('v' : rest) (x, y) (roboLoc, grid) = do
  let (newGrid, rl) = moveRobot' grid roboLoc D
  case rl of
    Just rl' -> handleChar' rest (0, 0) (rl', newGrid)
    _ -> handleChar' rest (0, 0) (roboLoc, newGrid)

getGPSCoordsSum' :: Grid' -> Int
getGPSCoordsSum' = Data.Map.foldrWithKey (\(x, y) v acc -> if isLeftBox' v (x, y) then acc + (y * 100 + x) else acc) 0

isLeftBox' :: Object' -> Location -> Bool
isLeftBox' (Box' (bx, _)) (lx, _) = lx < bx
isLeftBox' _ _ = False

renderGrid :: Grid' -> String
renderGrid grid = do
  let (bx, by) = Data.Map.foldrWithKey (\(x, y) _ (ax, ay) -> (max x ax, max y ay)) (0, 0) grid
  let locs = map (indexToLocation (bx + 1, by + 1)) (take ((bx + 1) * (by + 1)) [0, 1 ..])
  foldl (\acc l -> acc ++ (if fst l == 0 then "\n" else "") ++ case Data.Map.lookup l grid of Nothing -> ['.']; Just (Box' (_, _)) -> ['O']; Just Robot' -> ['@']; Just Wall' -> ['#']) "" locs