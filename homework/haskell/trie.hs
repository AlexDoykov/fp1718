data Tree a = Empty | Node (a, Maybe b) [(Tree a)] deriving (Show)
listTree :: (Ord a) => Tree a -> [a]
listTree Empty = []
listTree (Node root children) = root : listTree (head children)

add :: 