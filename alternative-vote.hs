import Data.List

votesConst :: [[String]]
votesConst = [["Red", "Green"], ["Blue"], ["Green", "Red", "Blue"], ["Blue", "Green", "Red"], ["Green"]]

votesConst2 :: [[String]]
votesConst2 = [["Red", "Green"], ["Blue"], ["Red", "Green", "Blue"], ["Blue", "Green", "Red"], ["Green"]]

votesConst3 :: [[String]]
votesConst3 = [["Red", "Green"], ["Red"], ["Red", "Green", "Blue"], ["Blue", "Green", "Red"], ["Green"]]

votesConst4 :: [[String]]
votesConst4 = [["Red", "Green"], ["Blue"], ["Red", "Green", "Blue"], ["Blue", "Green", "Red"], ["Blue"]]

countOccurrences :: (Eq a) => a -> [a] -> Int
countOccurrences y = length . filter (== y)

countStrings :: [String] -> [(String, Int)]
countStrings xs = [(x, countOccurrences x xs) | x <- nub xs, (not . null) x]

leastVotes :: [(String, Int)] -> [String]
leastVotes votes = [fst vote | vote <- votes, snd vote == minVoteNum]
  where
    minVoteNum = snd $ minimumBy (\a b -> compare (snd a) (snd b)) votes

getLosingCandidates :: [[String]] -> [String]
getLosingCandidates votes = (leastVotes . countStrings) firstRoundVotes
  where
    firstRoundVotes = [head v | v <- votes, (not . null) v]

removeCandidatesFromVotes :: [String] -> [[String]] -> [[String]]
removeCandidatesFromVotes toRemove = map (filter (`notElem` toRemove))

getFirstValue :: [[String]] -> String
getFirstValue votes = head $ head $ filter (not . null) votes

isVoteTied :: [[String]] -> Bool
isVoteTied = all null

isVoteComplete :: [[String]] -> Bool
isVoteComplete votes = any ((== 1) . length) votes && all (\a -> null a || a == [val]) votes
  where
    val = getFirstValue votes

applyVotingRound :: [[String]] -> [[String]]
applyVotingRound votes = removeCandidatesFromVotes (getLosingCandidates votes) votes

getWinner :: [[String]] -> String
getWinner votes
  | isVoteTied votes = "nobody"
  | isVoteComplete votes = getFirstValue votes
  | otherwise = getWinner (applyVotingRound votes)