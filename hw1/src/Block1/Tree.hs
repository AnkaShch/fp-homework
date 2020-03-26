module Block1.Tree
  ( Tree(..)
  , insertElem
  , isEmptyTree
  , findElem
  , fromList
  , removeElem
  , sizeTree
  ) where

data Tree a
  = Leaf
  | Node (Tree a) a [a] (Tree a)
  deriving (Show)

isEmptyTree :: Tree a -> Bool
isEmptyTree Leaf = True
isEmptyTree _    = False

findElem :: Ord a => a -> Tree a -> Maybe a
findElem _ Leaf = Nothing
findElem x (Node left elm _ right)
  | x == elm = Just elm
  | x < elm = findElem x left
  | otherwise = findElem x right

sizeTree :: Tree a -> Int
sizeTree Leaf = 0
sizeTree (Node left _ elms right) = 1 + sizeTree left + length elms + sizeTree right

insertElem :: Ord a => a -> Tree a -> Tree a
insertElem x Leaf = Node Leaf x [] Leaf
insertElem x (Node left elm elms right)
  | x == elm = Node left elm (x : elms) right
  | x < elm = Node (insertElem x left) elm elms right
  | otherwise = Node left elm elms (insertElem x right)

fromList :: Ord a => [a] -> Tree a
fromList = foldr insertElem Leaf

removeElem :: Ord a => a -> Tree a -> Tree a
removeElem _ Leaf = Leaf
removeElem x (Node left elm elms right)
  | x < elm = Node (removeElem x left) elm elms right
  | x > elm = Node left elm elms (removeElem x right)
  | otherwise =
    if not $ null elms
      then Node left elm (tail elms) right
      else merge left right
  where
    merge :: Tree a -> Tree a -> Tree a
    merge Leaf Leaf = Leaf
    merge leftChild Leaf = leftChild
    merge Leaf rightChild = rightChild
    merge (Node llChild leftElm leftElms lrChild) (Node rlChild rightElm rightElms rrChild) =
      Node llChild leftElm leftElms (Node (merge lrChild rlChild) rightElm rightElms rrChild)

instance Ord a => Eq (Tree a) where
  Leaf == Leaf = True
  _ == Leaf = False
  Leaf == _ = False
  (Node leftA elmA elmsA rightA) == (Node leftB elmB elmsB rightB) =
    (leftA == leftB) && (rightA == rightB) && (elmA == elmB) && (length elmsA == length elmsB)
