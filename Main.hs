import Lab2
import Data.Char

add :: (Int, Int) -> Int
add (x,y) = x+y

add' :: Int -> Int -> Int
add' x y = x+y

sum' :: Num a => [a] -> a
sum' (x:xs) = x + sum xs

abs' :: Int -> Int
abs' n | n >= 0    = n
       | otherwise = -n

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

sum100 :: Int
sum100 = sum [x^2 | x <- [1..100]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

find :: (Eq a) => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
    where n = length xs - 1

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x, y) <- xs `zip` ys]

-- xs = 1 : [x + 1 | x <- xs] ?????????

riffle :: [a] -> [a] -> [a]
riffle xs ys = concat [[x,y] | (x,y) <- xs `zip` ys]

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs : xss) = xs ++ concat xss