module Others.RecursiveDataStructures  
( Tree(..)
, singleton
, treeInsert
, treeElem
, reverseTree
) where

infixr 5 :-:  
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)  

infixr 5  .++  
(.++) :: List a -> List a -> List a   
Empty .++ ys = ys  
(x :-: xs) .++ ys = x :-: (xs .++ ys)  

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  

treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
            | x == a = Node x left right  
            | x < a  = Node a (treeInsert x left) right  
            | x > a  = Node a left (treeInsert x right)  

treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right  

reverseTree :: (Ord a) => Tree a -> Tree a
reverseTree EmptyTree = EmptyTree
reverseTree (Node a left right) = Node a right left