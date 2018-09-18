-- Let p(n) represent the number of different ways in which n coins can be 
-- separated into piles. For example, five coins can be separated into piles 
-- in exactly seven different ways, so p(5)=7.
-- 
--            OOOOO
--           OOOO   O
--          OOO   OO
--         OOO   O   O
--        OO   OO   O
--       OO   O   O   O
--      O   O   O   O   O
-- 
-- Find the least value of n for which p(n) is divisible by one million.
import Data.Array

-- 1, -1, 2, -2, 3, -3, 4, -4, ...
posAndNegNumbers = map f [0..]
    where f n = if mod n 2 == 0 then (div n 2) + 1 else -1 - (div n 2)

-- magic
cache :: Array Integer Integer
cache = array (0, 100000) [(x, p x) | x <- [0..100000]]

--partitions
p 0 = 1
p n | n<0       = 0
    | otherwise = sum $ zipWith (*) (map (cache!) $ takeWhile (0<=) [n-x | x<-map f posAndNegNumbers]) (cycle [1, 1, -1, -1])
        where f k = div (k * (3*k-1)) 2

main :: IO()
main = print $ head $ filter (\x->mod (p x) 1000000 == 0) [1..] 
