module Main where

import Lib

import System.Exit
import System.Environment(getArgs,getProgName)
import Text.Read




printVersion::String->String->IO()
printVersion appName appVersion=putStrLn $ appName++ " Version: "++appVersion


help::String->IO()
help appName=do
  putStrLn (appName++" <parameter> [<value>]")
  putStrLn "parameters:"
  putStrLn "\t\t-h/-help:\t help"
  putStrLn "\t\t-v:\t\t version"
  putStrLn "\t\t-f file:\thash file"
  putStrLn "\t\t-c hash:\thash function"
  putStrLn "\t\t-s string:\tplaintext"

parseArguments::[String]->String->IO()
parseArguments args appName=do
  case args of
    ["-h"]->help appName>>exitWith ExitSuccess
    ["-help"]->help appName>>exitWith ExitSuccess
    ["-v"]->printVersion appName "0.1.0.0">>exitWith ExitSuccess
    ["-f",value]->putStrLn value
    ["-c",value]->putStrLn value
    ["-s",value]->putStrLn value
    []->help appName>>exitWith (ExitFailure 1)


  
main :: IO ()
main = do
  args<-getArgs
  appName<-getProgName
  parseArguments args appName
