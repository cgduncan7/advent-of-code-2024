module Common (splitStringByFn, stringToPart, Part (P1, P2, Both), Runner, head', Location, Bounds, locationToIndex, indexToLocation, Dir (UL, U, UR, R, DR, D, DL, L), nothingIfOutOfBounds) where

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

nothingIfOutOfBounds :: Bounds -> Location -> Maybe Int
nothingIfOutOfBounds (boundX, boundY) (locX, locY) = do
  if locX < 0 || locY < 0 || locX >= boundX || locY >= boundY then Nothing else Just (locationToIndex (boundX, boundY) (locX, locY))

locationToIndex :: Bounds -> Location -> Int
locationToIndex (boundX, _) (locX, locY) = locY * boundX + locX

indexToLocation :: Bounds -> Int -> Location
indexToLocation (boundX, _) index = (index `mod` boundX, index `div` boundX)