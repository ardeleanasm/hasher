module CommandLineParser
  (
    parseArguments
  , RetValue(..)
  , RetType(..)
  , MessageType(..)
  )where

import HashGenerator

data RetType=ONLY_MESSAGE|HASH_PLAINTEXT|HASH_FILE|INTERACTIVE_MODE deriving (Eq,Show)
data MessageType=APP_HELP|APP_VERSION deriving (Eq,Show)


data RetValue=
  RetValue{
  hashFunction::Maybe HashFunction
  ,plaintext::String
  ,filePath::String
  ,messageType::Maybe MessageType
  }deriving (Eq,Show)

getFunction::String->Maybe HashFunction
getFunction functionName
  |functionName=="MD5"=Just MD5_F
  |functionName=="SHA1"=Just SHA1_F
  |functionName=="SHA256"=Just SHA256_F
  |functionName=="SHA512"=Just SHA512_F
  |otherwise=Just UNDEFINED_F


parseArguments::[String]->IO (RetType,RetValue)
parseArguments args=do
  case args of
    ["-h"]->
      return (ONLY_MESSAGE,RetValue{hashFunction=Nothing,plaintext="",filePath="",messageType=Just APP_HELP})
      --help appName>>exitWith ExitSuccess
    ["-help"]->
      return (ONLY_MESSAGE,RetValue{hashFunction=Nothing,plaintext="",filePath="",messageType=Just APP_HELP})
      --help appName>>exitWith ExitSuccess
    ["-v"]->
      return (ONLY_MESSAGE,RetValue{hashFunction=Nothing,plaintext="",filePath="",messageType=Just APP_VERSION})
      --printVersion appName "0.1.0.0">>exitWith ExitSuccess
    ["-f",hash,filePathString]->
      return (HASH_FILE,RetValue{hashFunction=(getFunction hash),plaintext="",filePath=filePathString,messageType=Nothing})
    ["-s",hash,plaintext]->
      return (HASH_PLAINTEXT,RetValue{hashFunction=(getFunction hash),plaintext=plaintext,filePath="",messageType=Nothing})
    []->
      return (INTERACTIVE_MODE,RetValue{hashFunction=Nothing,plaintext="",filePath="",messageType=Nothing})




