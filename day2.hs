import System.IO
import Control.Monad

day2_1 = do
    contents <- readFile "input_02.txt"
    let list = words contents
    let freqs = [(id,[(a,b) | (a,b) <- letterCounts id]) | id <- list]
    let twos = [(id,hastwos) | idc <- freqs, hastwos <- [getNs (snd idc) 2], id <- [fst idc]]
    let threes = [(id,hasthrees) | idc <- freqs, hasthrees <- [getNs (snd idc) 3], id <- [fst idc]]

    let checksum = (length [id | two <- twos, snd two]) * (length [id | three <- threes, snd three])
    putStrLn $ show checksum

day2_2 = do
    contents <- readFile "input_02.txt"
    let list = words contents
    let similarities = [ sims | s1 <- list, s2 <- list, sims <- [stringSimilarity s1 s2]]
    putStrLn $ head [sc | sc <- similarities, length sc == length (head list) - 1]

getNs :: [(Char,Int)] -> Int -> Bool
getNs freqs n
    | length freqs == 1 = snd (head freqs) == n
    | otherwise = or [getNs ([head freqs]) n,getNs (tail freqs) n]

letterCounts :: String -> [(Char,Int)]
letterCounts s = [(letter,freq) | letter <- ['a' .. 'z'], freq <- [letterCount s letter]]

letterCount :: String -> Char -> Int
letterCount s c
    | null s = 0
    | length s == 1 = if head s == c then 1 else 0
    | otherwise = letterCount ([head s]) c + letterCount (tail s) c

stringSimilarity :: String -> String -> String
stringSimilarity s1 s2 =
    [fst sc | sc <- zip s1 s2, fst sc == snd sc]
