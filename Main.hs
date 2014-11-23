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
