module Main (main) where

import Day2 (part1, part2)
import Test.Hspec

example1, example2 :: String
example1 = "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"
example2 =
  unlines
    [ "7 6 4 2 1",
      "1 2 7 8 9",
      "9 7 6 2 1",
      "1 3 2 4 5",
      "8 6 4 4 1",
      "1 3 6 7 9"
    ]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      part1 example1 `shouldBe` 2
  describe "part 2" $ do
    it "examples" $ do
      part2 example2 `shouldBe` 4