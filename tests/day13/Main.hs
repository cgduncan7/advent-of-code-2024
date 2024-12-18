module Main (main) where

import Day13 (part1, part2, solve)
import Test.Hspec

example1, example2, example3, example4, example5 :: String
example1 =
  unlines
    [ "Button A: X+94, Y+34",
      "Button B: X+22, Y+67",
      "Prize: X=8400, Y=5400",
      "",
      "Button A: X+26, Y+66",
      "Button B: X+67, Y+21",
      "Prize: X=12748, Y=12176",
      "",
      "Button A: X+17, Y+86",
      "Button B: X+84, Y+37",
      "Prize: X=7870, Y=6450",
      "",
      "Button A: X+69, Y+23",
      "Button B: X+27, Y+71",
      "Prize: X=18641, Y=10279"
    ]
example2 =
  unlines
    [ "Button A: X+94, Y+34",
      "Button B: X+22, Y+67",
      "Prize: X=8400, Y=5400"
    ]
example3 =
  unlines
    [ "Button A: X+26, Y+66",
      "Button B: X+67, Y+21",
      "Prize: X=12748, Y=12176"
    ]
example4 =
  unlines
    [ "Button A: X+17, Y+86",
      "Button B: X+84, Y+37",
      "Prize: X=7870, Y=6450"
    ]
example5 =
  unlines
    [ "Button A: X+69, Y+23",
      "Button B: X+27, Y+71",
      "Prize: X=18641, Y=10279"
    ]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solve" $ do
    it "should solve equations" $ do
      solve ((8400, 94, 22), (5400, 34, 67)) `shouldBe` Just (80, 40)
      solve ((8400, 94, 22), (5400, 34, 77)) `shouldBe` Nothing
  describe "part 1" $ do
    it "examples" $ do
      part1 example1 `shouldBe` 480
  describe "part 2" $ do
    it "examples" $ do
      part2 example2 `shouldBe` 0
      part2 example3 > 0 `shouldBe` True
      part2 example4 `shouldBe` 0
      part2 example5 > 0 `shouldBe` True
