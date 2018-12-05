import System.IO
import Control.Monad

day1_1 = do
    contents <- readFile "input_01.txt"
    let list = [readInt (formatFreq a) | a <- words contents]
    putStrLn $ show $ sum list

day1_2 = do
    contents <- readFile "input_01.txt"
    let list = cycle [readInt (formatFreq a) | a <- words contents]
    putStrLn $ show $ findFirstDuplicate list

formatFreq :: String -> String
formatFreq ('+' : x) = x
formatFreq x = x

readInt :: String -> Int
readInt x = read x :: Int

sumToK :: [Int] -> Int -> Int
sumToK l k
    | null l = error "list is empty."
    | k < 0 = error "k must be positive."
    | k == 1 = head l
    | otherwise = head l + sumToK (tail l) (k - 1)

findFirstDuplicate' :: [Int] -> [Int] -> Int -> Int -> (Int,Int)
findFirstDuplicate' encs cands k f
    | elem f encs = (f,k)
    | otherwise = findFirstDuplicate' (f : encs) cands (k + 1) (f + (cands !! k))

findFirstDuplicate :: [Int] -> (Int,Int)
findFirstDuplicate cands = findFirstDuplicate' [] cands 0 0
