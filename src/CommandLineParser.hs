module CommandLineParser
  (
    parseArguments
  , RetValue(..)
  , RetType(..)
  , MessageType(..)
  )where

import System.Exit

import Data.Text(pack)
import Data.Text.Encoding      (encodeUtf8)
import HashGenerator
import InteractiveRun

data RetType=ONLY_MESSAGE|HASH_PLAINTEXT|HASH_FILE|FAILURE deriving (Eq,Show)
data MessageType=APP_HELP|APP_VERSION deriving (Eq,Show)


data RetValue=
  RetValue{
  hashFunction::Maybe HashFunction
  ,plaintext::Maybe String
  ,file::Maybe String
  ,messageType::Maybe MessageType
  }deriving (Eq,Show)



parseArguments::[String]->IO (RetType,RetValue)
parseArguments args=do
  case args of
    ["-h"]->return (ONLY_MESSAGE,RetValue{hashFunction=Nothing,plaintext=Nothing,file=Nothing,messageType=Just APP_HELP})
      --help appName>>exitWith ExitSuccess
    ["-help"]->return (ONLY_MESSAGE,RetValue{hashFunction=Nothing,plaintext=Nothing,file=Nothing,messageType=Just APP_HELP})
      --help appName>>exitWith ExitSuccess
    ["-v"]->return (ONLY_MESSAGE,RetValue{hashFunction=Nothing,plaintext=Nothing,file=Nothing,messageType=Just APP_VERSION})
      --printVersion appName "0.1.0.0">>exitWith ExitSuccess
    ["-i"]->return (ONLY_MESSAGE,RetValue{hashFunction=Nothing,plaintext=Nothing,file=Nothing,messageType=Just APP_VERSION})
      --runInteractive
    ["-f",hash,file]->return (ONLY_MESSAGE,RetValue{hashFunction=Nothing,plaintext=Nothing,file=Nothing,messageType=Just APP_VERSION})
      --hashFile hash file
    ["-s",hash,plaintext]->return (ONLY_MESSAGE,RetValue{hashFunction=Nothing,plaintext=Nothing,file=Nothing,messageType=Just APP_VERSION})
      --hashPlaintext hash plaintext
    []->return (ONLY_MESSAGE,RetValue{hashFunction=Nothing,plaintext=Nothing,file=Nothing,messageType=Just APP_VERSION})
      --help appName>>exitWith (ExitFailure 1)



hashPlaintext::String->String->IO()
hashPlaintext hash plaintext=print $ calculate (what hash) $ encodeUtf8 $ pack plaintext where
      what "MD5"=MD5_F
      what "SHA1"=SHA1_F
      what "SHA256"=SHA256_F
      what "SHA512"=SHA512_F
      what _=UNDEFINED_F
  
hashFile::String->String->IO()
hashFile hash file=putStrLn $concat [hash," ",file]
