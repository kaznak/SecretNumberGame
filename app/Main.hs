module Main where

import SecretNumberGame

main :: IO ()
main = do
  putStrLn "Input Seed Number"
  sn <- doinit
  putStrLn "Start"
  checkLoop sn (Hint 0 0)

doinit :: IO SecretNum
doinit = readLn >>= return . mkSecretNum

checkLoop :: SecretNum -> Hint -> IO ()
checkLoop sn (Hint 4 0) = do
  putStrLn "Congratulation!!"
  print sn
checkLoop sn _ = do
  ns <- readLn
  ht <- return $ checkSecret sn ns
  print ht
  checkLoop sn ht
