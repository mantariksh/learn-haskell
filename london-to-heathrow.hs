import System.IO

type Distance = Int

type Path = String

type PathAndDistance = (Path, Distance)

type JunctionData = (Distance, Distance, Distance)

type DistanceData = [JunctionData]

data NodeType = A | B

minPathAndDistance :: PathAndDistance -> PathAndDistance -> PathAndDistance
minPathAndDistance a b = if snd a < snd b then a else b

appendPathAndDistance :: PathAndDistance -> PathAndDistance -> PathAndDistance
appendPathAndDistance a b = (fst a ++ fst b, snd a + snd b)

minDistanceFromNode :: NodeType -> PathAndDistance -> DistanceData -> PathAndDistance
minDistanceFromNode nodeType soFar [(dA, dB, dC)] =
  case nodeType of
    A -> appendPathAndDistance soFar ("A", dA)
    B -> appendPathAndDistance soFar ("B", dB)
minDistanceFromNode nodeType soFar ((dA, dB, dC) : remainingDistances) =
  let straightPath = case nodeType of
        A -> appendPathAndDistance soFar ("A", dA)
        B -> appendPathAndDistance soFar ("B", dB)
      straightPathRemainingDistance = minDistanceFromNode nodeType straightPath remainingDistances
      crossPath = case nodeType of
        A -> appendPathAndDistance soFar ("AC", dA + dC)
        B -> appendPathAndDistance soFar ("BC", dB + dC)
      oppositeNodeType = case nodeType of
        A -> B
        B -> A
      crossPathRemainingDistance = minDistanceFromNode oppositeNodeType crossPath remainingDistances
   in minPathAndDistance straightPathRemainingDistance crossPathRemainingDistance

londonToHeathrow :: DistanceData -> PathAndDistance
londonToHeathrow distanceData = minPathAndDistance (minDistanceFromNode A ("", 0) distanceData) (minDistanceFromNode B ("", 0) distanceData)

splitArrayIntoThrees :: [a] -> [(a, a, a)]
splitArrayIntoThrees (x : y : z : zs) = (x, y, z) : splitArrayIntoThrees zs
splitArrayIntoThrees _ = []

stringToIntArray :: String -> [Int]
stringToIntArray s = map read $ lines s

main = do
  withFile
    "london-to-heathrow.txt"
    ReadMode
    ( \handle -> do
        contents <- hGetContents handle
        print ((londonToHeathrow . splitArrayIntoThrees . stringToIntArray) contents)
    )