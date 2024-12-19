module Main (main) where

import Day14 (part1, part2)
import Test.Hspec

example1, example2 :: String
example1 =
  unlines
    [ "p=0,4 v=3,-3",
      "p=6,3 v=-1,-3",
      "p=10,3 v=-1,2",
      "p=2,0 v=2,-1",
      "p=0,0 v=1,3",
      "p=3,0 v=-2,-2",
      "p=7,6 v=-1,-3",
      "p=3,0 v=-1,-2",
      "p=9,3 v=2,3",
      "p=7,3 v=-1,2",
      "p=2,4 v=2,-3",
      "p=9,5 v=-3,-3"
    ]
example2 = ""

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      part1 example1 `shouldBe` 12 -- ans > 103605480
  describe "part 2" $ do
    it "examples" $ do
      part2 example2 `shouldBe` 2
