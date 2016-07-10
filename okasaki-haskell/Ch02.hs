module Ch02 where



-- Exercice 2.2

-- member performs 2d comparison -> improve to do <= d+1 comparison, d = depth of the tree

data BSTree a = Empty | Node (BSTree a) a (BSTree a)
                       deriving (Show, Eq)

member :: (Ord a) => a -> BSTree a -> Bool
member _ Empty = False
member x (Node l v r)
  | x < v = member x l
  | x > v = member x r
  | otherwise = True

insert :: (Ord a) => a -> BSTree a -> BSTree a
insert x Empty = Node Empty x Empty
insert x (Node l v r)
  | x < v = Node (insert x l) v r
  | v < x = Node l v (insert x r)
  | otherwise = (Node l x r)


-- a first attempt at a version with less comparisons
member2 :: (Ord a) => a -> BSTree a -> Bool
member2 _ Empty = False
member2 x t@(Node _ v _) = member' t v
   where member' Empty c = x == c
         member' (Node a y b) c =
           if x < y then member' a c
           else member' b y

-- another attempt at a version with less comparison
member3 :: (Ord a) => a -> BSTree a -> Bool
member3 x t = member3' x t Nothing
    where member3' y Empty (Just v) = y == v
          member3' _ Empty Nothing = False
          member3' y (Node l v r) m = if y < v then member3' y l m
                                      else member3' y r (Just v)

