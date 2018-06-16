module Main where

import System.Exit
import System.Environment(getArgs,getProgName)
import CommandLineParser

import Data.Text(pack)
import Data.Text.Encoding      (encodeUtf8)
import HashGenerator
import InteractiveRun
import Data.Maybe

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


hashPlaintext::String->[Char]->IO()
hashPlaintext hash plaintext=print $ calculate (what hash) $ encodeUtf8 $ pack plaintext where
      what "MD5"=MD5_F
      what "SHA1"=SHA1_F
      what "SHA256"=SHA256_F
      what "SHA512"=SHA512_F
      what _=UNDEFINED_F
  
hashFile::String->String->IO()
hashFile hash file=putStrLn $concat [hash," ",file]


  
main :: IO ()
main = do
  args<-getArgs
  appName<-getProgName
  header
  arguments<-parseArguments args
  case arguments of
    (ONLY_MESSAGE,secondArgument)->do
      case messageType secondArgument of
        Just APP_HELP->help appName>>exitWith ExitSuccess
        Just APP_VERSION->printVersion appName "0.1.0.0">>exitWith ExitSuccess
    (FAILURE,_)->help appName>>exitWith (ExitFailure 1)
    (HASH_PLAINTEXT,secondArgument)->
      hashPlaintext (show $ fromJust $ hashFunction secondArgument) (fromJust $ plaintext secondArgument)>>exitWith ExitSuccess
  putStrLn "Finished"
  
