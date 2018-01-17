-- Bst - Binary Search Tree
-- типът очаква като аргументи два други типа, че да стане използваем:
-- тип за ключовете на дървото
-- и тип за стойностите
-- Например: Bst Int String е дърво с целочислени ключове и има низове за стойности
-- Както при обикновеното двоично дърво, имаме два случая - Или имаме празно дърво,
-- или имаме връх със стойност, ляво и дясно поддърво
-- Стойностите за нас са двойка от ключ и стойност
-- Свободни сте да променяте тази дефиниция на типа, стига промяната да улесни живота ви.
data Bst key value = Empty | Node (key,value) (Bst key value) (Bst key value) deriving Show

-- Търсим стойността на даден ключ (от тип а) в дадено дърво
-- (с ключове от тип а и стойности от тип b).
-- Тъй като търсеният ключ може да не съществува в дървото,
-- резултатът на функцията е Maybe b
search :: (Ord a) => a -> Bst a b -> Maybe b 
search _ Empty = Nothing
search x (Node pair t1 t2)
    | fst pair == x = (Just (snd pair))
    | fst pair < x = search x t2
    | otherwise = search x t1
--Искаме да вмъкнем дадена двойка (ключ, стойност) в дадено двоично дърво
-- и да върнем новото дърво като резултат
insert :: (Ord a) => (a,b) -> Bst a b -> Bst a b
insert x Empty = (Node x Empty Empty)
insert x (Node root left right)
    | fst root > fst x = Node root (insert x left) right
    | fst root < fst x = Node root left (insert x right)
    | fst root == fst x = Node root left right
-- Обхождаме дадено двоично дърво и трупаме в резултатен списък
inOrder :: (Ord a) => Bst a b -> [(a,b)]
inOrder Empty = []
inOrder (Node root left right) = ((inOrder left)++[root])++(inOrder right)
-- Обхождаме дадено двоично дърво и трупаме в резултатен списък
preOrder :: (Ord a) => Bst a b -> [(a,b)]
preOrder Empty = []
preOrder (Node root left right) = root : (preOrder left) ++ (preOrder right)

-- Обхождаме дадено двоично дърво и трупаме в резултатен списък
traverse' :: (Ord a) => Bst a b -> [(a,b)]
traverse' Empty = []
traverse' (Node root left right) = (traverse' right) ++ (traverse' left) ++ [root]

-- Като допълнителна, но незадължителна задача, може да реализирате изтриване
-- на връх с даден ключ от дадено дърво-}

build :: (Ord a) => [(a, b)] -> Bst a b
build xs = foldr insert Empty xs




example1 :: Bst Int String
example1 = Node (5,"A") 
            (Node (3,"A") 
                (Node (1,"A") 
                  Empty 
                  Empty) 
                (Node (4,"A") 
                    Empty 
                    Empty)) 
            (Node (7,"A") 
                (Node (6,"A") 
                    Empty 
                    Empty) 
                (Node (8,"A") 
                    Empty 
                    Empty))



