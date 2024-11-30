module Main where

import Common (Part (Both, P1, P2), Runner, stringToPart)
import Day0
import System.Console.GetOpt
import System.Environment (getArgs)

data Flag = Test | Part String deriving (Eq, Show)

newtype Options = Options {optPart :: Part} deriving (Show)

defaultOptions :: Options
defaultOptions =
  Options {optPart = Both}

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['p'] ["part"] (ReqArg (\d opts -> opts {optPart = stringToPart d}) "part") "choose part to execute"
  ]

getOpts :: [String] -> IO (Options, String)
getOpts argv =
  case getOpt Permute options argv of
    (o, [n], []) -> return (foldl (flip id) defaultOptions o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: ec -p <day>"

hasOption :: Flag -> [Flag] -> Bool
hasOption ef (f : rest) = (ef == f) || hasOption ef rest
hasOption _ [] = False

getFileName :: String -> String -> String
getFileName s p = "./data/day" ++ s ++ "/data" ++ p ++ ".txt"

emptyRunner :: Runner
emptyRunner _ _ = []

getDay :: String -> Runner
getDay "0" = Day0.run
getDay _ = emptyRunner

main :: IO ()
main = do
  args <- getArgs
  (Options {optPart}, day) <- getOpts args
  let fileNames =
        ( case optPart of
            P1 -> [getFileName day "1"]
            P2 -> [getFileName day "2"]
            _ -> [getFileName day "1", getFileName day "2"]
        )
  contents <- mapM readFile fileNames
  print $ getDay day optPart contents