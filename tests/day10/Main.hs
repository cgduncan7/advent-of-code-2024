module Main (main) where

import Day10 (part1, part2)
import Test.Hspec

example1, example2 :: String
example1 =
  unlines
    [ "89010123",
      "78121874",
      "87430965",
      "96549874",
      "45678903",
      "32019012",
      "01329801",
      "10456732"
    ]
example2 =
  unlines
    [ "89010123",
      "78121874",
      "87430965",
      "96549874",
      "45678903",
      "32019012",
      "01329801",
      "10456732"
    ]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "part 1" $ do
    it "examples" $ do
      part1 example1 `shouldBe` 36
  describe "part 2" $ do
    it "examples" $ do
      part2 example2 `shouldBe` 81
