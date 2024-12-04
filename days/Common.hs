module Common (splitStringByFn, stringToPart, Part (P1, P2, Both), Runner, head', Location, Bounds, locationToIndex, indexToLocation) where

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

locationToIndex :: Bounds -> Location -> Int
locationToIndex (boundX, _) (locX, locY) = locY * boundX + locX

indexToLocation :: Bounds -> Int -> Location
indexToLocation (boundX, _) index = (index `mod` boundX, index `div` boundX)