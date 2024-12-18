module Main (main) where

import Day12 (part1, part2)
import Test.Hspec

example1, example2 :: String
example1 =
  unlines
    [ "RRRRIICCFF",
      "RRRRIICCCF",
      "VVRRRCCFFF",
      "VVRCCCJFFF",
      "VVVVCJJCFE",
      "VVIVCCJJEE",
      "VVIIICJJEE",
      "MIIIIIJJEE",
      "MIIISIJEEE",
      "MMMISSJEEE"
    ]
example2 =
  unlines
    [ "AAAA",
      "BBCD",
      "BBCC",
      "EEEC"
    ]

example3 =
  unlines
    [ "AAAAAA",
      "AAABBA",
      "AAABBA",
      "ABBAAA",
      "ABBAAA",
      "AAAAAA"
    ]

example4 =
  unlines
    [ "EEEEE",
      "EXXXX",
      "EEEEE",
      "EXXXX",
      "EEEEE"
    ]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      part1 example2 `shouldBe` 140
      part1 example1 `shouldBe` 1930
  describe "part 2" $ do
    it "examples" $ do
      part2 example3 `shouldBe` 368
      part2 example4 `shouldBe` 236
      part2 example1 `shouldBe` 1206
