import Control.Concurrent
import System.Directory.Internal.Prelude (getArgs)

type Row = [Bool]

type Board = [Row]

type Position = (Int, Int)

type NeighbourFinder = Board -> Position -> Position

board1 :: Board
board1 =
  [ [False, False, False, False, False],
    [False, False, False, True, False],
    [False, True, False, True, False],
    [False, False, True, True, False],
    [False, False, False, False, False]
  ]

move :: Board -> (Int -> Int) -> (Int -> Int) -> Position -> Position
move b f1 f2 (x, y) = (f1 x `mod` numCols, f2 y `mod` numRows)
  where
    numRows = length b
    numCols = length (head b)

topLeft :: NeighbourFinder
topLeft b = move b (subtract 1) (subtract 1)

top :: NeighbourFinder
top b = move b id (subtract 1)

topRight :: NeighbourFinder
topRight b = move b (+ 1) (subtract 1)

left :: NeighbourFinder
left b = move b (subtract 1) id

right :: NeighbourFinder
right b = move b (+ 1) id

bottomLeft :: NeighbourFinder
bottomLeft b = move b (subtract 1) (+ 1)

bottom :: NeighbourFinder
bottom b = move b id (+ 1)

bottomRight :: NeighbourFinder
bottomRight b = move b (+ 1) (+ 1)

neighbours :: Board -> Position -> [Position]
neighbours b p = [topLeft b p, top b p, topRight b p, left b p, right b p, bottomLeft b p, bottom b p, bottomRight b p]

valAtPos :: Board -> Position -> Int
valAtPos b p = if isAlive b p then 1 else 0

isAlive :: Board -> Position -> Bool
isAlive b (x, y) = (b !! y) !! x

numAliveNeighbours :: Board -> Position -> Int
numAliveNeighbours b p = sum $ map (valAtPos b) (neighbours b p)

doesLive :: Board -> Position -> Bool
doesLive b p
  | isAlive b p = ns == 2 || ns == 3
  | otherwise = ns == 3
  where
    ns = numAliveNeighbours b p

positions :: Board -> [[Position]]
positions b = [[(x, y) | x <- [0 .. numCols - 1]] | y <- [0 .. numRows - 1]]
  where
    numRows = length b
    numCols = (length . head) b

playRound :: Board -> Board
playRound b = map (map (doesLive b)) (positions b)

playRounds :: Int -> Board -> [Board]
playRounds n b = take (n + 1) (iterate playRound b)

showCell :: Bool -> IO ()
showCell b = if b then putChar '*' else putChar '-'

showRow :: [Bool] -> IO ()
showRow r = do
  mapM_ showCell r
  putChar '\n'

showBoard :: Board -> IO ()
showBoard = mapM_ showRow

showBoardAndDelay :: Int -> Board -> IO ()
showBoardAndDelay ms b = do
  showBoard b
  putStrLn ""
  threadDelay ms

showRound :: Int -> Board -> IO ()
showRound n b = showBoard $ last $ playRounds n b

showSeries :: Int -> Board -> IO ()
showSeries n b = mapM_ (showBoardAndDelay 500000) (playRounds n b)

main = do
  args <- getArgs
  showSeries (read (head args)) board1