import Data.List (sort)

fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . iterate map'
  where map' = \n -> if even n then n `div` 2 else 3 * n + 1

---------------------

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

insert :: a -> Tree a -> Tree a
insert node Leaf = Node 0 Leaf node Leaf
insert node (Node h left n right)
  | height left < height right = Node h left' n right
  | otherwise = Node h' left n right'
  where
    height Leaf = -1
    height (Node h _ _ _) = h
    left'  = insert node left
    right' = insert node right
    h' = 1 + maximum(height left, height right')

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

---------------------

xor :: [Bool] -> Bool
xor xs = foldl (\x acc -> if x then not acc else acc) False xs

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f(x) : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x acc -> f acc x) base xs

---------------------

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 2 : map (\x -> x * 2 + 1) included
  where
    included = concat $ map (\(start, end) -> [start+1..end-1]) includedIntervals
    includedIntervals = zip (0 : excluded) excluded
    excluded = sort [fsToNum i j | i <- [1..n], j <- [i..n], fsToNum i j <= n `div` 2]
    fsToNum i j = i + j + 2 * i * j
