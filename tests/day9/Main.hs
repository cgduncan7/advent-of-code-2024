module Main (main) where

import Day9 (part1, part2)
import Test.Hspec

example1, example2 :: String
example1 = "2333133121414131402"
example2 = "2333133121414131402"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      part1 example1 `shouldBe` 1928
  describe "part 2" $ do
    it "examples" $ do
      part2 example2 `shouldBe` 2
