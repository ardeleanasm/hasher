module InteractiveRun
  (
    runInteractive
  )
where
import System.IO
import HashGenerator
import Data.Text(pack)
import Data.Text.Encoding      (encodeUtf8)


foo::String->IO()
foo str=putStrLn str

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
  return (case value of
             "MD5"->MD5_F
             "SHA1"->SHA1_F
             "SHA256"->SHA256_F
             "SHA512"->SHA512_F
             _->UNDEFINED_F)


  
  

runInteractive::IO()
runInteractive=do
  hSetBuffering stdout NoBuffering
  chooseHashFunctionMenu
  hashFunction<-chooseHashFunction
  putStr "Enter Plaintext:"
  plaintext<-getLine
  print $ calculate hashFunction $ encodeUtf8 $ pack plaintext



