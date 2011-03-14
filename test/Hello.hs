{-# LANGUAGE NoImplicitPrelude #-}
module Hello where

import Prelude hiding (map, take)

map f [] = []
map f (x:xs) = f x : map f xs

-- inc = map (+1)

-- foo = let zig x = x:zag x
--           zag x = x:zig x
--       in zig True

-- mapFst f [] = []
-- mapFst f ((y,z):xs) = (f y, z):mapFst f xs

data Nat = Z | S Nat

n `plus` Z = n
n `plus` (S m) = S (n `plus` m)

take Z xs = []
take _ [] = []
take (S n) (x:xs) = x:take n xs

main = Z `plus` Z
