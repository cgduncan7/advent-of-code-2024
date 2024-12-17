module Main (main) where

import Day11 (blink, blinkTimes, blinkTimesWithMemoization, part1, part2)
import Debug.Trace (traceShow)
import Test.Hspec

example1, example2 :: String
example1 = "125 17"
example2 = "125 17"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "helpers" $ do
    it "blink" $ do
      blink [0, 1, 10, 99, 999] `shouldBe` [1, 2024, 1, 0, 9, 9, 2021976]
    it "blinkTimesWithMemoization" $ do
      blinkTimesWithMemoization [0, 1, 10, 99, 999] 2 `shouldBe` map (,0) [2024, 20, 24, 2024, 1, 18216, 18216, 4092479424]
      blinkTimesWithMemoization [125, 17] 6 `shouldBe` map (,0) [2097446912, 14168, 4048, 2, 0, 2, 4, 40, 48, 2024, 40, 48, 80, 96, 2, 8, 6, 7, 6, 0, 3, 2]
      traceShow (blinkTimesWithMemoization [125, 17] 7) blinkTimesWithMemoization [125, 17] 8 `shouldBe` map (,0) (blinkTimes [125, 17] 8)
  describe "part 1" $ do
    it "examples" $ do
      part1 example1 `shouldBe` 55312
