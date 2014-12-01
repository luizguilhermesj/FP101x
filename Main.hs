import Lab2
import Data.Char
import Data.Bits

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

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = foldr (||) True (map p xs)
-- any' p xs = foldr (\x acc -> (p x) || acc) False xs
-- any' p xs = not (all (\x ->  not (p x)) xs)
-- any' p = null . filter p
-- any' p = not . null . dropWhile (not . p)
-- any' p xs = length (filter p xs) > 0
-- any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x = x : takeWhile' p xs
  | otherwise = []
-- takeWhile' p = foldl (\acc x -> if p x then x : acc else acc) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldl (\ xs x -> xs ++ [f x]) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\ x xs -> if p x then x : xs else xs) []

dec2int :: [Integer] -> Integer
dec2int = foldl (\x y -> 10 * x + y) 0

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

-- sumsqreven = compose [sum, map (^2), filter even]

curry :: ((a, b) -> c) -> a -> b -> c
curry f = \x y -> f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = \ (x, y) -> f x y

chop8 :: [Integer] -> [[Integer]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

chop8' :: [Integer] -> [[Integer]]
chop8' = unfold null (take 8) (drop 8)

map2 :: (a -> b) -> [a] -> [b]
map2 f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (const False) id f

double :: Integer -> Integer
double n = n * 2

triple :: Integer -> Integer
triple n = n * 3

test' :: Integer -> Bool
test' = even . double

test2 :: Integer -> Bool
test2 = even . triple

test3 :: Integer -> Bool
test3 = even . (double . triple)

test4 :: Integer -> Bool
test4 = (even . double) . triple
