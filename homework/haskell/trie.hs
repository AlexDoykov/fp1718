data Tree key value = Empty | Node (key, Maybe value) [(Tree key value)] deriving (Show)
{-listTree :: (Ord a) => Tree a -> [a]
listTree Empty = []
listTree (Node root children) = root : listTree (head children)-}
{-add :: (a, Maybe b) -> Tree a -> (a, Maybe b)
add x (Node root children) = if root == x then root else x
-}


bools :: Int -> [[Bool]]
bools 0 = []
bools 1 = [[True], [False]]
bools n = [True:x | x <- bools (n - 1)] ++ [False:x | x <- bools (n - 1)]