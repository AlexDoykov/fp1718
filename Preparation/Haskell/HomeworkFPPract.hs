import Data.Map

duplicateElements :: [a] -> [a]
duplicateElements [] = []
duplicateElements (x:xs) = x : x : duplicateElements xs

slice :: Int -> Int -> [a] -> [a]
slice start end xs = take (end - start + 1) $ drop start xs

isPrime :: Integer -> Bool
isPrime x = if length [y | y <- [2 .. x - 1] , x `mod` y == 0] > 0 then False else True

primeNumbers :: Integer -> [Integer]
primeNumbers n = [x | x <- [3 .. n] , isPrime x == True]

findCompliment :: Integer -> [Integer] -> Integer -> Maybe (Integer, Integer)
findCompliment _ [] _ = Nothing
findCompliment m (x:xs) n = if m + x == n then  Just (m, x) else findCompliment m xs n

helper :: [Integer] -> Integer -> Maybe (Integer, Integer)
helper (x : xs) n
    | n `mod` 2 /= 0 = Nothing
    | findCompliment x xs n == Nothing = helper  xs n
    | otherwise = findCompliment x xs n

primeSum :: Integer -> Maybe (Integer, Integer)
primeSum n = helper (primeNumbers n) n

hailstone :: Integer -> [Integer]
hailstone 1 = [1]
hailstone n = if n `mod` 2 == 0 then n : hailstone (n `div` 2) else n : hailstone (3 * n + 1)

oddNotPrimeNumbers = [x | x <- [10 .. 99] , x `mod` 2 /= 0, isPrime x == False]
squaresPowByTwo = [2 * x * x | x <- [1 .. 7]]

listCorrectNumbers :: [Integer] -> [Integer] -> [Integer] -> [Integer]
listCorrectNumbers [] _ _ = []
listCorrectNumbers xs (y : ys) [] = listCorrectNumbers xs ys (primeNumbers 100)
listCorrectNumbers (x : xs) [] _ = listCorrectNumbers xs squaresPowByTwo (primeNumbers 100) 
listCorrectNumbers list0@(x : xs) list1@(y : ys) (z : zs) 
    | x == y + z = x : listCorrectNumbers xs squaresPowByTwo (primeNumbers 100)
    | otherwise = listCorrectNumbers list0 list1 zs

countNumbers :: Int
countNumbers = length (listCorrectNumbers oddNotPrimeNumbers squaresPowByTwo (primeNumbers 100))

intercalate :: [a] -> [[a]] -> [a]
intercalate _ [] = []
intercalate [] xs = concat xs
intercalate y list@(x:xs) = if length list == 1 
                            then concat list
                        else (x++y)++intercalate y xs