module Common (splitStringByFn, stringToPart, Part (P1, P2, Both), Runner, head', Location, Bounds, locationToIndex, indexToLocation, Dir (UL, U, UR, R, DR, D, DL, L), nothingIfOutOfBounds, move) where

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

type Location = (Int, Int)

type Bounds = Location

data Dir = UL | U | UR | R | DR | D | DL | L deriving (Show)

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