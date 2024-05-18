module Main where

import Data.Char (chr, ord)
import Utils

ex1a :: String
ex1a = show [truncate $ 11 ** x | x <- [0 .. 6]]

ex1b :: String
ex1b = show [x | x <- [1 .. 40], rem x 4 /= 0]

ex1c :: String
ex1c = show ["A" ++ [chr x] ++ "BB" | x <- [ord 'a' .. ord 'g']]

ex1e :: String
ex1e = show [2 ** x | x <- reverse [-5 .. 0]]

ex1f :: String
ex1f = show [9 * x + 1 | x <- [0 .. 7]]

ex2 :: String -> Bool
ex2 str = length str `rem` 2 == 0

ex3 :: [String] -> [String]
ex3 xs = reverse xs

ex4 :: [String] -> [Int]
ex4 xs = filter odd $ map length xs

ex5 :: [a] -> a
ex5 xs = last . reverse $ xs

splitAtHalf :: [a] -> ([a], [a])
splitAtHalf [] = ([], [])
splitAtHalf xs = splitAt (length xs `div` 2) xs

reverseSnd :: ([a], [a]) -> ([a], [a])
reverseSnd (l, r) = (l, reverse r)

compareTuple :: (Eq a) => (a, a) -> Bool
compareTuple (l, r) = l == r

dropMiddle :: [a] -> [a]
dropMiddle [] = []
dropMiddle xs =
  l ++ drop 1 r
  where
    (l, r) = splitAtHalf xs

ex6 :: String -> Bool
ex6 s
  | even $ length s = compareTuple . reverseSnd $ splitAtHalf s
  | otherwise = compareTuple . reverseSnd $ splitAtHalf $ dropMiddle s

ex7 :: Int -> (Int, Int, Int, Int)
ex7 i = (2 * i, 3 * i, 4 * i, 5 * i)

main :: IO ()
main = do
  title "# Ex 2.1"
  runMany [ex1a, ex1b, ex1c, ex1e, ex1f]

  title "# Ex 2.2"
  printLn $ ex2 "Teste"
  printLn $ ex2 "Testee"

  title "# Ex 2.3"
  printLn $ ex3 ["a", "b", "c"]

  title "# Ex 2.4"
  printLn $ ex4 ["a", "aa", "aaa", "aaaa", "aaaaa"]

  title "# Ex 2.5"
  printLn $ ex5 ["a", "aa", "aaa", "aaaa", "aaaaa"]

  title "# Ex 2.6"
  printLn $ map ex6 ["arara", "banana", "110010011"]

  title "# Ex 2.7"
  printLn $ ex7 1