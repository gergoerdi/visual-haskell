{-# LANGUAGE NoImplicitPrelude #-}
module Hello where

import Prelude hiding (map)

map f [] = []
map f (x:xs) = f x : map f xs

inc = map (+1)

foo = let zig x = x:zag x
          zag x = x:zig x
      in zig True

-- mapFst f [] = []
-- mapFst f ((y,z):xs) = (f y, z):mapFst f xs
