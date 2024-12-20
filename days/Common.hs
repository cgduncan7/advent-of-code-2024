module Common (splitStringByFn, stringToPart, Part (P1, P2, Both), Runner, filterMaybes, head', last', Location, Bounds, locationToIndex, indexToLocation, Dir (UL, U, UR, R, DR, D, DL, L), nothingIfOutOfBounds, nothingIfLocOutOfBounds, move, move', distance, pathTo, rest, isInteger, oppositeDir) where

import Data.List (elemIndex)

type Runner = Part -> [String] -> [Int]

data Part = P1 | P2 | Both deriving (Eq, Show)

stringToPart :: String -> Part
stringToPart "1" = P1
stringToPart "2" = P2
stringToPart _ = Both

partToChar :: Part -> Char
partToChar P1 = '1'
partToChar P2 = '2'
partToChar _ = 'A'

-- Splits a string into substrings by running a function on each character
-- If the function returns true, it splits
splitStringByFn :: String -> (Char -> Bool) -> [String]
splitStringByFn s f = case dropWhile f s of
  "" -> []
  s' -> w : splitStringByFn s'' f
    where
      (w, s'') = break f s'

-- head that returns `Maybe a`
head' :: [a] -> Maybe a
head' [] = Nothing
head' (a : as) = Just a

last' :: [a] -> Maybe a
last' [] = Nothing
last' x = head' $ reverse x

type Location = (Int, Int)

type Bounds = Location

data Dir = UL | U | UR | R | DR | D | DL | L deriving (Eq, Show)

-- Dir are given a value started from 0 at UL and going clockwise for no good reason
dirVals = [UL, U, UR, R, DR, D, DL, L] :: [Dir]

oppositeDir :: Dir -> Dir
oppositeDir d = case elemIndex d dirVals of
  Just i -> dirVals !! ((i + 4) `mod` 8)
  Nothing -> error "something bad"

instance Ord Dir where
  (<) a b = elemIndex a dirVals < elemIndex b dirVals
  (<=) a b = elemIndex a dirVals <= elemIndex b dirVals
  (>) a b = elemIndex a dirVals > elemIndex b dirVals
  (>=) a b = elemIndex a dirVals >= elemIndex b dirVals
  max a b = if a >= b then a else b
  min a b = if a >= b then b else a

move :: Bounds -> Location -> Dir -> Maybe Location
move bounds (x, y) U = nothingIfLocOutOfBounds bounds (x, y - 1)
move bounds (x, y) UL = nothingIfLocOutOfBounds bounds (x - 1, y - 1)
move bounds (x, y) UR = nothingIfLocOutOfBounds bounds (x + 1, y - 1)
move bounds (x, y) D = nothingIfLocOutOfBounds bounds (x, y + 1)
move bounds (x, y) DL = nothingIfLocOutOfBounds bounds (x - 1, y + 1)
move bounds (x, y) DR = nothingIfLocOutOfBounds bounds (x + 1, y + 1)
move bounds (x, y) R = nothingIfLocOutOfBounds bounds (x + 1, y)
move bounds (x, y) L = nothingIfLocOutOfBounds bounds (x - 1, y)

move' :: Location -> Dir -> Location
move' (x, y) U = (x, y - 1)
move' (x, y) UL = (x - 1, y - 1)
move' (x, y) UR = (x + 1, y - 1)
move' (x, y) D = (x, y + 1)
move' (x, y) DL = (x - 1, y + 1)
move' (x, y) DR = (x + 1, y + 1)
move' (x, y) R = (x + 1, y)
move' (x, y) L = (x - 1, y)

nothingIfLocOutOfBounds :: Bounds -> Location -> Maybe Location
nothingIfLocOutOfBounds (boundX, boundY) (locX, locY) = do
  if locX < 0 || locY < 0 || locX >= boundX || locY >= boundY then Nothing else Just (locX, locY)

nothingIfOutOfBounds :: Bounds -> Location -> Maybe Int
nothingIfOutOfBounds (boundX, boundY) (locX, locY) = do
  if locX < 0 || locY < 0 || locX >= boundX || locY >= boundY then Nothing else Just (locationToIndex (boundX, boundY) (locX, locY))

locationToIndex :: Bounds -> Location -> Int
locationToIndex (boundX, _) (locX, locY) = locY * boundX + locX

indexToLocation :: Bounds -> Int -> Location
indexToLocation (boundX, _) index = (index `mod` boundX, index `div` boundX)

distance :: Location -> Location -> Float
distance (ax, ay) (bx, by) = (((fromIntegral bx :: Float) - (fromIntegral ax :: Float)) + ((fromIntegral by :: Float) - (fromIntegral ay :: Float))) / 2

pathTo :: Location -> Location -> (Int, Int)
pathTo (ax, ay) (bx, by) = (bx - ax, by - ay)

rest :: [a] -> [a]
rest (_ : r) = r

-- remove maybes
filterMaybes :: [Maybe a] -> [a]
filterMaybes (Nothing : rest) = filterMaybes rest
filterMaybes (Just x : rest) = x : filterMaybes rest
filterMaybes [] = []

isInteger :: Double -> Bool
isInteger f = f == fromInteger (round f)