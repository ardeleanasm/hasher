module Main where


import System.Environment(getArgs,getProgName)
import CommandLineParser











  
main :: IO ()
main = do
  args<-getArgs
  appName<-getProgName
  parseArguments args appName
