import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero
         | Succ Nat
         deriving Show

natToInteger :: Nat -> Integer
natToInteger = undefined -- \n -> length [c | c <- show n, c == 'S']

integerToNat :: Integer -> Nat
integerToNat (n+1) = let m = integerToNat n in Succ m
integerToNat 0 = Zero


add :: Nat -> Nat -> Nat
add n Zero = n
add n (Succ m) = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

{-
data Tree = Leaf Integer
            | Node Tree Integer Tree

occurs :: Integer -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r)
  | m == n = True
  | m < n = occurs m l
  | otherwise = occurs m r


occurs m (Leaf n) = m == n
occurs m (Node l n r)
  = case compare m n of
        LT -> occurs m r
        EQ -> True
        GT -> occurs m l
-}

data Tree = Leaf Integer
            | Node Tree Tree

leaves :: Tree -> Integer
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r

balanced :: Tree -> Bool
balanced (Leaf _) = True
balanced (Node l r)
  = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

halve :: [Integer] -> ([Integer], [Integer])
halve xs = splitAt (length xs `div` 2) xs
balance :: [Integer] -> Tree
balance [x] = Leaf x
balance xs = Node (balance ys) (balance zs)
  where (ys, zs) = halve xs

{-
data Nat = Zero | Succ Nat

x = Succ (Succ Zero)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add m n = int2nat (nat2int m + nat2int n)

data Expr = Val Int
  | Add Expr Expr
  | Mul Expr Expr


eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

eval' :: Expr -> Int
eval' = foldr id (+) (*)
-}
