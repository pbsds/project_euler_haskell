-- The following iterative sequence is defined for the set of positive integers:
-- 
--     n → n/2 (n is even)
--     n → 3n + 1 (n is odd)
-- 
-- Using the rule above and starting with 13, we generate the following sequence:
-- 
-- 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
-- 
-- It can be seen that this sequence (starting at 13 and finishing at 1)
-- contains 10 terms. Although it has not been proved yet (Collatz Problem),
-- it is thought that all starting numbers finish at 1.
-- Which starting number, under one million, produces the longest chain?
-- NOTE: Once the chain starts the terms are allowed to go above one million.

step n | even n = div n 2
       | odd n  = 3*n + 1

collatzLength start = asd 1 start
    where
        asd i 1 = i
        asd i n = asd (i+1) (step n)

main :: IO()
main = print $ fst $ foldr asd (1, 1) $ map (\n->(n, collatzLength n)) [1..999999]
    where asd (i, n) (i2, n2) = if n > n2 then (i, n) else (i2, n2)
