module Main where

import System.Exit
import System.IO
import System.Environment(getArgs,getProgName)
import CommandLineParser
import Control.Monad(forever,when)
import Data.Text(pack)
import Data.Text.Encoding      (encodeUtf8)
import HashGenerator
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

chooseHashFunctionMenu::IO()
chooseHashFunctionMenu=do
  putStrLn "Available Hash Functions:"
  putStrLn "\t1. MD5"
  putStrLn "\t2. SHA1"
  putStrLn "\t3. SHA256"
  putStrLn "\t4. SHA512"
  putStr "Select Hash Function:"


chooseHashFunction::IO HashFunction
chooseHashFunction=do
  value<-getLine
  return (selectHashFunction (read value::Int))

selectHashFunction::Int->HashFunction
selectHashFunction value
  |value==1=MD5_F
  |value==2=SHA1_F
  |value==3=SHA256_F
  |value==4=SHA512_F
  |otherwise=UNDEFINED_F

printHashingOptions::IO()  
printHashingOptions=putStrLn "1. Hash plaintext" >> putStrLn "2. Hash File"


readHashingOption::IO Int
readHashingOption=do
  value<-getLine
  return (read value::Int)

runInteractive::IO()
runInteractive=forever $ do
  hSetBuffering stdout NoBuffering
  chooseHashFunctionMenu
  hashFunction<-chooseHashFunction
  printHashingOptions
  val<-readHashingOption
  case val of
    1->do
      putStr "Enter Plaintext:"
      plaintext<-getLine
      hashPlaintext (show hashFunction) plaintext
    2->putStrLn "Ana are mere"
    _->exitWith (ExitFailure 1)
  putStrLn "1. Continue\n2. Exit"
  value<-getLine
  when (not $ ((read value::Int)==1))$ do exitWith ExitSuccess

 
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
    (HASH_PLAINTEXT,secondArgument)->
      hashPlaintext (show $ fromJust $ hashFunction secondArgument) (fromJust $ plaintext secondArgument)>>exitWith ExitSuccess
    (HASH_FILE,secondArgument)->
      hashFile (show $ fromJust $ hashFunction secondArgument) (show $ fromJust $ file secondArgument)>>exitWith ExitSuccess
    (INTERACTIVE_MODE,_)->
      runInteractive
  putStrLn "Finished"
  
