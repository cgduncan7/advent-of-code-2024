module Day0 (run, part1, part2) where

import Common (Part (P1, P2), Runner)

run :: Runner
run P1 [p1Contents] = [part1 p1Contents]
run P2 [p2Contents] = [part2 p2Contents]
run _ [p1Contents, p2Contents] = [part1 p1Contents, part2 p2Contents]

part1 :: String -> Int
part1 _ = 2

part2 :: String -> Int
part2 _ = 2
