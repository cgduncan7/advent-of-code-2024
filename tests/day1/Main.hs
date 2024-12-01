module Main (main) where

import Day1 (part1, part2)
import Test.Hspec

example1, example2 :: String
example1 = "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"
example2 = "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      part1 example1 `shouldBe` 11
  describe "part 2" $ do
    it "examples" $ do
      part2 example2 `shouldBe` 31
