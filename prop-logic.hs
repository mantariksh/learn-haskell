import Data.List (nub)

data Prop
  = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Imply Prop Prop

t1 :: Prop
t1 = Var 'A' `And` Not (Var 'A')

t2 :: Prop
t2 = (Var 'A' `And` Var 'B') `Imply` Var 'A'

t3 :: Prop
t3 = Var 'A' `Imply` (Var 'A' `And` Var 'B')

t4 :: Prop
t4 = (Var 'A' `And` (Var 'A' `Imply` Var 'B')) `Imply` Var 'B'

getCharValue :: (Eq a) => [(a, b)] -> a -> b
getCharValue m k = head [snd el | el <- m, fst el == k]

isTrue :: Prop -> [(Char, Bool)] -> Bool
isTrue (Const b) m = b
isTrue (Var c) m = getCharValue m c
isTrue (Not p) m = not $ isTrue p m
isTrue (p1 `And` p2) m = isTrue p1 m && isTrue p2 m
isTrue (p1 `Imply` p2) m = not (isTrue p1 m) || isTrue p2 m

gatherChars :: Prop -> [Char]
gatherChars (Const b) = []
gatherChars (Var c) = [c]
gatherChars (Not p) = gatherChars p
gatherChars (p1 `And` p2) = gatherChars p1 ++ gatherChars p2
gatherChars (p1 `Imply` p2) = gatherChars p1 ++ gatherChars p2

allCombinationBools :: Int -> [[Bool]]
allCombinationBools 0 = []
allCombinationBools 1 = [[True], [False]]
allCombinationBools n = map (True :) bss ++ map (False :) bss
  where
    bss = allCombinationBools (n - 1)

allCombinationBoolsForChars :: [Char] -> [[(Char, Bool)]]
allCombinationBoolsForChars cs = [zip cs bs | bs <- allCombinationBools (length cs)]

isTautology :: Prop -> Bool
isTautology p = and [isTrue p m | m <- (allCombinationBoolsForChars . nub . gatherChars) p]
