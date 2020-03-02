module Tree
( Tree(..)
, singleton
, treeInsert
, treeHasElement
, buildTree
) where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeHasElement :: (Ord a) => a -> Tree a -> Bool
treeHasElement x EmptyTree = False
treeHasElement x (Node a left right)
    | x == a = True
    | x < a = treeHasElement x left
    | x > a = treeHasElement x right

buildTree :: (Ord a) => [a] -> Tree a
buildTree = foldr treeInsert EmptyTree

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)
