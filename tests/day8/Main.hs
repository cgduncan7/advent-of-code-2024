module Main (main) where

import Day8 (part1, part2)
import Test.Hspec

example1a, example1b, example2a, example2b :: String
example1a =
  unlines
    [ "..........",
      "..........",
      "..........",
      "....a.....",
      "..........",
      ".....a....",
      "..........",
      "..........",
      "..........",
      ".......a.."
    ]
example1b =
  unlines
    [ "............",
      "........0...",
      ".....0......",
      ".......0....",
      "....0.......",
      "......A.....",
      "............",
      "............",
      "........A...",
      ".........A..",
      "............",
      "............"
    ]
example2a =
  unlines
    [ "T.........",
      "...T......",
      ".T........",
      "..........",
      "..........",
      "..........",
      "..........",
      "..........",
      "..........",
      ".........."
    ]
example2b =
  unlines
    [ "............",
      "........0...",
      ".....0......",
      ".......0....",
      "....0.......",
      "......A.....",
      "............",
      "............",
      "........A...",
      ".........A..",
      "............",
      "............"
    ]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      part1 example1a `shouldBe` 2
      part1 example1b `shouldBe` 14
  describe "part 2" $ do
    it "examples" $ do
      part2 example2a `shouldBe` 9
      part2 example2b `shouldBe` 34
