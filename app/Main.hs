module Main where

import HashGenerator

import System.Exit
import System.Environment(getArgs,getProgName)
import Data.Text(pack)
import Data.Text.Encoding      (encodeUtf8)




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

  


parseArguments::[String]->String->IO()
parseArguments args appName=do
  header
  case args of
    ["-h"]->help appName>>exitWith ExitSuccess
    ["-help"]->help appName>>exitWith ExitSuccess
    ["-v"]->printVersion appName "0.1.0.0">>exitWith ExitSuccess
    ["-i"]->runInteractive
    ["-f",hash,file]->hashFile hash file
    ["-s",hash,plaintext]->hashPlaintext hash plaintext
    []->help appName>>exitWith (ExitFailure 1)

hashPlaintext::String->String->IO()
hashPlaintext hash plaintext=print $ calculate (what hash) $ encodeUtf8 $ pack plaintext where
      what "MD5"=MD5_F
      what "SHA1"=SHA1_F
      what "SHA256"=SHA256_F
      what "SHA512"=SHA512_F
      what _=UNDEFINED_F
  
hashFile::String->String->IO()
hashFile hash file=putStrLn $concat [hash," ",file]

runInteractive::IO()
runInteractive=undefined

  
main :: IO ()
main = do
  args<-getArgs
  appName<-getProgName
  parseArguments args appName
