{-# LANGUAGE NoImplicitPrelude #-}
module Hello where

import Prelude hiding (map)

map f [] = []
map f (x:xs) = f x : map f xs

mapFst f [] = []
mapFst f ((y,z):xs) = (f y, z):mapFst f xs
