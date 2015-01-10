isZero :: Integer -> Bool
isZero n | n /= 0 = False
isZero 0 = True

last' :: [a] -> a
last' [x] = x
last' (_ : xs) = last xs

(***) :: [a] -> [a] -> [a]
(x : xs) *** ys = x : (xs *** [])
[] *** ys = ys
