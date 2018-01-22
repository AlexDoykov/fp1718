takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
    | f x = x : takeWhile f xs
    | otherwise = []


dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs)
    | f x = dropWhile' f xs
    | otherwise = x:xs

dropWhile'' :: (a -> Bool) -> [a] -> [a]
dropWhile'' _ [] = []
dropWhile'' f list@(x:xs)
    | f x = dropWhile'' f xs
    | otherwise = list

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] ->[b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

zip'' :: [a] -> [b] -> [(a,b)]
zip'' = zipWith' (,)

lessThan :: Ord a => a -> [a] -> [a]
lessThan y xs = [x | x <- xs , x < y]

lessThan' :: Ord a => a -> [a] -> [a]
lessThan' y = filter (<y)

everyNth'' :: Int -> [a] -> [a]
everyNth'' n xs = map (xs !!) [n-1,2*n-1..size]
    where size = (length xs - length xs `mod` n)
everyNth' :: Int -> [a] -> [a]
everyNth' n xs = map fst [x | x <- zip xs [1..length xs], (snd x) `mod` n == 0]

isDescending :: Int -> Bool
isDescending n
    | n < 10 = True
    | n `mod` 10 > (n `div` 10) `mod` 10 = False
    | otherwise = isDescending (n `div` 10)

digits :: Integer -> [Int]
digits n = map (read . return) (show n)

stupid :: Integer -> Double
stupid = helper 1

helper :: Integer -> Integer -> Double
helper start end = if start > 2*end+1 then 0.0 else sqrt(fromIntegral (start) + helper (2*start+1) end)

anotherStupid = [3^x | x <- [1 .. 30]]