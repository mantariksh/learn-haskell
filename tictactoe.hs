import Data.Char
import Data.List

data Player = O | B | X deriving (Show, Ord, Eq)

type Pos = (Int, Int)

type Row = [Player]

type Board = [Row]

type State = (Player, Board)

size :: Int
size = 3

empty :: Board
empty = replicate size (replicate size B)

board1 :: Board
board1 = [[O, B, X], [O, X, B], [X, O, B]]

getDiags :: Board -> [[Player]]
getDiags b =
  [[b !! n !! n | n <- [0 .. length b - 1]], [b !! (length b - x - 1) !! x | x <- [0 .. length b - 1]]]

isWinner :: Player -> Board -> Bool
isWinner p b = any (all (== p)) (rows ++ cols ++ diags)
  where
    rows = b
    cols = transpose b
    diags = getDiags b

getWinner :: Board -> Player
getWinner b
  | isWinner O b = O
  | isWinner X b = X
  | otherwise = B

isFull :: Board -> Bool
isFull = all (notElem B)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 x (y : ys) = x : ys
replaceAt n x (y : ys) = y : replaceAt (n - 1) x ys

trd3 :: (a, b, c) -> c
trd3 (_, _, x) = x

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

-- This can be modified to get ideal position
nextPos :: Player -> Board -> Pos
nextPos _ b = head [(fst3 x, snd3 x) | x <- index b, trd3 x == B]
  where
    index b = concat [[(x, y, b !! y !! x) | x <- [0 .. length b - 1]] | y <- [0 .. length b - 1]]

move :: Player -> Pos -> Board -> Board
move p (x, y) b = replaceAt y (replaceAt x p (b !! y)) b

nextPlayer :: Player -> Player
nextPlayer O = X
nextPlayer X = O
nextPlayer B = B

isFinished :: State -> Bool
isFinished (_, b) = isFull b || getWinner b /= B

playComputerMove :: State -> State
playComputerMove (p, b) = (nextPlayer p, move p (nextPos p b) b)

interleave :: a -> [a] -> [a]
interleave _ [] = []
interleave _ [y] = [y]
interleave x (y : ys) = y : x : interleave x ys

showPlayer :: Player -> String
showPlayer X = "X"
showPlayer O = "O"
showPlayer B = " "

showRow :: Row -> String
showRow r = concat $ interleave " | " (map showPlayer r)

showBoard :: Board -> String
showBoard b = unlines (map showRow b)

showState :: State -> String
showState (_, b) = showBoard b

printBoard :: Board -> IO ()
printBoard b = putStr (showBoard b)

printGame :: [State] -> IO ()
printGame states = putStr (concat $ interleave "\n\n" $ map showState states)

getInt :: String -> IO Int
getInt prompt = do
  putStr prompt
  input <- getLine
  if (not . null) input && all isDigit input
    then return (read input)
    else do
      putStrLn "Error: invalid number!"
      getInt prompt

isValid :: Pos -> Board -> Bool
isValid (x, y) b = x < length b && y < length b && b !! y !! x == B

getMove :: Player -> Board -> IO Pos
getMove p b = do
  putStrLn (show p ++ " to move.")
  y <- getInt "Row: "
  x <- getInt "Column: "
  if isValid (x, y) b
    then return (x, y)
    else do
      putStrLn "Invalid move!\n"
      getMove p b

playComputerComputer :: IO ()
playComputerComputer = do
  firstMove <- getMove O empty
  let rounds = iterate playComputerMove (X, move O firstMove empty)
  printGame $ takeWhile (not . isFinished) rounds ++ [head (filter isFinished rounds)]

playHumanComputer :: State -> IO ()
playHumanComputer (p, b) = do
  humanMove <- getMove p b
  let nextState = (X, move O humanMove b)
  putStrLn (showState nextState)
  if isFinished nextState
    then return ()
    else do
      let followingState = playComputerMove nextState
      putStrLn (showState followingState)
      if isFinished followingState then return () else playHumanComputer followingState

playHumanHuman :: State -> IO ()
playHumanHuman (p, b) = do
  nextMove <- getMove p b
  let nextState = (nextPlayer p, move p nextMove b)
  putStrLn (showState nextState)
  if isFinished nextState
    then return ()
    else playHumanHuman nextState

main = playComputerComputer