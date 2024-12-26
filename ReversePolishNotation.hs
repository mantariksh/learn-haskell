import Data.Char

calculateReversePolish :: String -> Float
calculateReversePolish s = 0

-- tokeniseInput :: String -> String[]
-- tokeniseInput

handleNextInputChar :: [String] -> Char -> [String]
handleNextInputChar acc c
  | null acc = [[c]]
  | isSpace c = acc
  | isDigit c && isDigit lastChar = init acc ++ [last acc ++ [c]]
  | isDigit c = acc ++ [[c]]
  | isMultiCharStartOperator c = 
  | isSingleCharOperator c = acc ++ [[c]]
  where
    lastChar = (last . last) acc

isSingleCharOperator :: Char -> Bool
isSingleCharOperator c = c `elem` "+-/*^"

isMultiCharStartOperator :: Char -> Bool
-- Start of "ln" or "sum"
isMultiCharOperator c = c `elem` "ls"

main = do
  input <- getLine
  print (calculateReversePolish input)