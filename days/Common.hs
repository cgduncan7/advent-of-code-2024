module Common (splitStringByFn, stringToPart, Part (P1, P2, Both), Runner, head', last', Location, Bounds, locationToIndex, indexToLocation, Dir (UL, U, UR, R, DR, D, DL, L), nothingIfOutOfBounds, nothingIfLocOutOfBounds, move, distance, pathTo, rest) where

import Data.List (elemIndex, findIndex)

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