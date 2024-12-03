module Day3 (run, part1, part2) where

import Common (Part (P1, P2), Runner)
import Data.Foldable (foldl')
import Debug.Trace (traceShow)
import Text.Regex.TDFA (getAllTextMatches, (=~))

run :: Runner
run P1 [p1Contents] = [part1 p1Contents]
run P2 [p2Contents] = [part2 p2Contents]
run _ [p1Contents, p2Contents] = [part1 p1Contents, part2 p2Contents]

part1 :: String -> Int
part1 input = do
  let cmds = getAllTextMatches (input =~ mulCmdPattern) :: [String]
  sum $ map executeMul cmds

part2 :: String -> Int
part2 input = do
  let cmds = getAllTextMatches (input =~ allCmdsPattern) :: [String]
  traceShow cmds (handleCmds cmds True 0)

mulCmdPattern = "mul\\([0-9]{1,3},[0-9]{1,3}\\)" :: String

doCmdPattern = "do\\(\\)" :: String

dontCmdPattern = "don't\\(\\)" :: String

allCmdsPattern = "(" ++ mulCmdPattern ++ "|" ++ doCmdPattern ++ "|" ++ dontCmdPattern ++ ")" :: String

mulOperandsPattern = "[0-9]{1,3}" :: String

executeMul :: String -> Int
executeMul cmd = do
  foldl' (*) 1 $ map (\s -> read s :: Int) (getAllTextMatches (cmd =~ mulOperandsPattern) :: [String])

isMul :: [Char] -> Bool
isMul ('m' : rest) = True
isMul _ = False

isDo :: [Char] -> Bool
isDo ('d' : 'o' : '(' : rest) = True
isDo _ = False

isDont :: [Char] -> Bool
isDont ('d' : 'o' : 'n' : rest) = True
isDont _ = False

handleCmds :: [String] -> Bool -> Int -> Int
handleCmds (hcmd : rcmds) mulEnabled acc = do
  if isMul hcmd && mulEnabled
    then handleCmds rcmds mulEnabled (acc + executeMul hcmd)
    else
      if isMul hcmd && not mulEnabled
        then handleCmds rcmds mulEnabled acc
        else
          if isDo hcmd
            then handleCmds rcmds True acc
            else handleCmds rcmds False acc
handleCmds [] _ acc = acc