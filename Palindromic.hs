module Palindromic
    where

import Data.Char
--import Control.Monad.Trans.Writer

isPalindrome :: Integer -> Bool
isPalindrome n = nS == reverse nS
    where nS = show n

type Count = Int
doucetteConst = 315

addRev :: Integer -> Integer
addRev n = n + (read . reverse . show) n

palindromize' :: Count -> Integer -> (Count, Integer)    
palindromize' k n
  | isPalindrome n = (k, n)
  | otherwise = palindromize' (k + 1)
                              (addRev n)

palindromize :: Integer -> (Count, Integer)
palindromize = palindromize' 0

isLyshrel' :: Count -> Integer -> Bool
isLyshrel' k n
    | k > doucetteConst = True
    | isPalindrome n = False
    | otherwise = isLyshrel' (k + 1) (addRev n)

isLyshrel1 :: Integer -> Bool
isLyshrel1 n = isLyshrel' 0 (addRev n)

palindromizeIO' :: Count -> Integer -> IO (Count, Integer)
palindromizeIO' k n
  | isPalindrome n 
    {-|| k > 100 -} = return (k, n)
  | otherwise = 
      do putStrLn $ show (k, n)
         palindromizeIO' (k + 1)
                       (addRev n)
                       
palindromizeIO = palindromizeIO' 0