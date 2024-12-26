module Tree (emptyTree, insert, member) where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

emptyTree :: (Ord a) => Tree a
emptyTree = EmptyTree

insert :: (Ord a) => a -> Tree a -> Tree a
insert x EmptyTree = Node x emptyTree emptyTree
insert x (Node y left right)
  | x < y = Node y (insert x left) right
  | otherwise = Node y left (insert x right)

member :: (Ord a) => a -> Tree a -> Bool
member x EmptyTree = False
member x (Node y left right)
  | x == y = True
  | x < y = member x left
  | otherwise = member x right
