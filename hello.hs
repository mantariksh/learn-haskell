import Control.Monad
import Data.Char
import System.IO

main = do
  contents <- readFile "words.txt"
  putStr $ (unlines . map toPalindromeResult . lines) contents
  where
    isPalindrome s = s == reverse s
    toPalindromeResult a = if isPalindrome a then "palindrome" else "not a palindrome"
