import Data.Char

simple x y = if x^2 + y^2 == 50 then 10000 else 300

sumBetween :: Int->Int->Int
sumBetween start end = foldr (+) 0 [start .. end]

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib(n - 1) + fib(n - 2)

fact :: Int->Int
fact n = product [1..n]

fold' :: (a->a->a)->a->[a]->a
fold' _ null_value [] = null_value
fold' f null_value (x:xs) = f x (fold' f null_value xs)



append' :: [a] -> [a] -> [a]
append' = foldr (:)


removeNonUppercase :: [Char] -> [Char]
removeNonUppercase xs = [x | x <- xs, isUpper x == True]

sumthree :: Int -> Int -> Int -> Int
sumthree x y z = x + y + z

sum' :: (Num a) => [a] -> a
sum' = foldr (+) 0

length' :: [a] -> Integer
length' = foldr count' 0

count' :: a -> Integer -> Integer
count' x n = n + 1

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!" 


max' :: (Ord a) => a -> a -> a
max' x y
    | x > y = x
    | otherwise = y




removeElem' :: (Eq a) => [a] -> a -> [a]
removeElem' xs y = [x | x <- xs, x /= y]


aritm :: [Double] -> Double
aritm [] = 0
aritm xs = sum' xs / (fromIntegral (length xs))


insert' :: (Ord a) => (a -> a -> Bool) -> a -> [a] -> [a]
insert' _ x [] = [x]
insert' f x (y:xs)
    | f x y = x:y:xs
    |otherwise = y:insert' f x xs



divisors :: Int -> [Int]
divisors y = [x | x <- [1 .. y], y `rem` x == 0 ]


prime :: Int -> Bool
prime x = divisors x == [1,x]


{-

insert2 :: (Ord a) => ([a] -> [a])
insert2 = insert' 2-}


takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
    | f x = x : (takeWhile' f xs)
    | otherwise = []




dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs)
    | not (f x) = x : (dropWhile' f xs)
    | otherwise = (dropWhile' f xs)



zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ []  = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys




zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ []  = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys



zipUseZipWith' :: [a] -> [b] -> [(a, b)]
zipUseZipWith' =  zipWith (,)




nthElem :: Int -> [a] -> [a]
nthElem n xs = if (n > (length xs)) then [] else (head (drop (n-1) xs)) : nthElem n (drop n xs)



insertionSort :: (Ord a) => (a -> a -> Bool) -> [a] -> [a]
insertionSort f = foldr (insert' f) []


{-
getFirstElement :: (a -> Bool) -> [a] ->  Maybe a
getFirstElement pred  = if empty filter pred  then head . filter pred else Nothing-}


applyToMaybe :: (a -> b) -> Maybe a -> Maybe b
applyToMaybe _ Nothing = Nothing
applyToMaybe f (Just x) = Just (f x)



map'' :: (a -> b) -> [a] -> [b]
map'' _ [] = []
map'' f (x:xs) = f x : map'' f xs 

data List a = Nil | Cons a (List a) deriving (Show)


length'' :: List a -> Int
length'' Nil = 0
length'' (Cons x xs) = 1 + length'' xs

foldr' :: (a -> b -> b) -> b -> List a -> b
foldr' _ null_value Nil = null_value
foldr' f null_value (Cons x xs) =  f x (foldr' f null_value xs)


foldl' :: (a -> b -> a) -> a -> List b -> a
foldl' _ null_value Nil = null_value
foldl' f null_value (Cons x xs) =  f (foldl' f null_value xs) x 

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

count :: Tree a -> Int
count Empty = 0
count (Node x t1 t2) = (1 + (count t1) + (count t2)) 

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Empty = Empty
treeMap f (Node x t1 t2) = (Node (f x) (treeMap f t1) (treeMap f t2))


{-treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x Empty = (Node x (Empty) (Empty))
treeInsert x (r tree1 tree2)
    | x < r = treeInsert x tree1
    | x > r = treeInsert x tree2
    | otherwise = 

-}
 































