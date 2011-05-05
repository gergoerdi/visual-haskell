module Main where

up_list :: Integer -> Integer -> Integer -> [Integer]
up_list x0 delta lim = go (x0 :: Integer)
  where
    go x | x > lim   = []
         | otherwise = x : go (x+delta)

test = (head . tail . tail) $ up_list 1 1 10
-- test = up_list 1 1 10

main = return ()
