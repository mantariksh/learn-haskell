import Data.List
import Data.Map qualified as Map

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x * 2

thrd :: (a, b, c) -> c
thrd (_, _, z) = z

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_ : xs) = 1 + length' xs

compare' :: (Ord a) => a -> a -> String
compare' x y
  | x > y = "x is greater"
  | x == y = "they are the same"
  | otherwise = "y is greater"

max' :: (Ord a) => [a] -> a
max' [] = error "Can't max an empty list"
max' [x] = x
max' (x : xs)
  | x > maxOfRest = x
  | otherwise = maxOfRest
  where
    maxOfRest = max' xs

replicate' :: Int -> a -> [a]
replicate' n x
  | n < 0 = error "Can't replicate negative times"
  | n == 0 = []
  | otherwise = x : replicate' (n - 1) x

take' :: Int -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x : xs) =
  leftElements ++ [x] ++ rightElements
  where
    leftElements = quicksort' (filter (<= x) xs)
    rightElements = quicksort' (filter (> x) xs)

largestDivisible = head (filter divisibleBy3829 [100000, 99999 ..])
  where
    divisibleBy3829 x = x `mod` 3829 == 0

sumOddSquaresLessThan10000 = sum (takeWhile (< 10000) (filter odd (map (^ 2) [1 ..])))

collatzSequence :: Int -> [Int]
collatzSequence 1 = [1]
collatzSequence x = x : collatzSequence collatzed
  where
    collatzed = if odd x then x * 3 + 1 else x `div` 2

chainLengthsGreaterThan15 = length (filter (> 15) (map (length . collatzSequence) [1 .. 100]))

sumFoldL :: (Num a) => [a] -> a
sumFoldL = foldl (+) 0

elemFoldL :: (Ord a) => a -> [a] -> Bool
elemFoldL a = foldl (\acc next -> acc || a == next) False

mapFoldR :: (a -> b) -> [a] -> [b]
mapFoldR f = foldr (\x acc -> f x : acc) []

maximumFoldL :: (Ord a) => [a] -> a
maximumFoldL = foldl1 (\acc next -> if acc > next then acc else next)

reverseFoldL :: [a] -> [a]
reverseFoldL = foldl (flip (:)) []

productFoldR :: (Num a) => [a] -> a
productFoldR = foldr1 (*)

filterFoldR :: (a -> Bool) -> [a] -> [a]
filterFoldR f = foldr (\x acc -> if f x then x : acc else acc) []

headFoldR :: [a] -> a
headFoldR = foldr1 (\x _ -> x)

lastFoldL :: [a] -> a
lastFoldL = foldl1 (\_ x -> x)

numStepsForRootsToExceed1000 = length (takeWhile (<= 1000) (scanl1 (+) (map sqrt [1 ..]))) + 1

sumOddSquaresSmallerThan10000 = sum . takeWhile (< 10000) . map (^ 2) . filter odd $ [1 ..]

oddSquareSum :: Int
oddSquareSum =
  let oddSquares = map (^ 2) $ filter odd [1 ..]
      belowLimit = takeWhile (< 10000) oddSquares
   in sum belowLimit

sublist' :: (Eq a) => [a] -> [a] -> Bool
sublist' needle haystack =
  let needleLen = length needle
   in foldl (\acc x -> acc || (take needleLen x == needle)) False (tails haystack)

data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

nudgePoint :: Point -> Float -> Float -> Point
nudgePoint (Point x y) dx dy = Point (x + dx) (y + dy)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle c r) dx dy = Circle (nudgePoint c dx dy) r
nudge (Rectangle p1 p2) dx dy = Rectangle (nudgePoint p1 dx dy) (nudgePoint p2 dx dy)

data Car = Car
  { company :: String,
    modelName :: String,
    year :: Int
  }
  deriving (Show)

c = Car "Ford" "Mustang" 1997

x = year c

type Code = String

data LockerStatus = Taken | Free deriving (Show, Eq)

type LockerMap = Map.Map Int (LockerStatus, Code)

getLocker :: Int -> LockerMap -> Either String Code
getLocker x lockerMap =
  case Map.lookup x lockerMap of
    Nothing -> Left "Locker number does not exist"
    Just (status, code) -> if status == Taken then Left "Locker is taken" else Right code

class Truthy a where
  (!!!) :: a -> Bool

instance Truthy String where
  (!!!) "" = False
  (!!!) _ = True

instance Truthy Int where
  (!!!) 0 = False
  (!!!) _ = True

instance Truthy Double where
  (!!!) 0 = False
  (!!!) _ = True

instance Truthy Integer where
  (!!!) 0 = False
  (!!!) _ = True

instance Truthy Float where
  (!!!) 0 = False
  (!!!) _ = True

instance Truthy (Maybe a) where
  (!!!) Nothing = False
  (!!!) _ = True
