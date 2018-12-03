import System.IO
import Control.Monad

day1_1 = do
    contents <- readFile "input_01.txt"
    let list = [readInt (formatFreq a) | a <- words contents]
    putStrLn (show(sum list))

formatFreq :: String -> String
formatFreq ('+':x) = x
formatFreq x = x

readInt :: String -> Int
readInt x = read x :: Int
