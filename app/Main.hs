module Main where


import System.Environment(getArgs,getProgName)
import CommandLineParser




printVersion::String->String->IO()
printVersion appName appVersion=putStrLn $ appName++ " Version: "++appVersion


help::String->IO()
help appName=do
  putStrLn (appName++" <parameter> [<value>]")
  putStrLn "parameters:"
  putStrLn "\t\t-h/-help\t help"
  putStrLn "\t\t-v\t\t version"
  putStrLn "\t\t-i\t\t interactive"
  putStrLn "\t\t-f <hash function abbreviation> <file>"
  putStrLn "\t\t-s <hash function abbreviation> <plaintext>"
  putStrLn "\t_________________________________________________________________"
  putStrLn "\t|\tImplemented Hash Functions\t|\tAbbreviation\t|"
  putStrLn "\t|\t\t MD5 \t\t\t|\t\tMD5\t|"
  putStrLn "\t|\t\t SHA1 \t\t\t|\t\tSHA1\t|"
  putStrLn "\t|\t\t SHA256 \t\t|\t\tSHA256\t|"
  putStrLn "\t|\t\t SHA512 \t\t|\t\tSHA512\t|"
  putStrLn "\t-----------------------------------------------------------------"

header::IO()
header=do
  putStrLn "  _    _           _               "
  putStrLn " | |  | |         | |              "
  putStrLn " | |__| | __ _ ___| |__   ___ _ __ "
  putStrLn " |  __  |/ _` / __| '_ \\ / _ \\ '__|"
  putStrLn " | |  | | (_| \\__ \\ | | |  __/ |   "
  putStrLn " |_|  |_|\\__,_|___/_| |_|\\___|_|   "





  
main :: IO ()
main = do
  args<-getArgs
  appName<-getProgName
  header
  arguments<-parseArguments args
  putStrLn "Ana"
  
