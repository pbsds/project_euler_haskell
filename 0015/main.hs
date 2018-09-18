-- Starting in the top left corner of a 2×2 grid, and only being able 
-- to move to the right and down, there are exactly 6 routes to the 
-- bottom right corner.
-- How many such routes are there through a 20×20 grid?
-- 
-- https://projecteuler.net/problem=15

factorial 0 = 1
factorial n = n * factorial (n - 1)

combination n k | k <= n    = div (factorial n) (factorial k * factorial (n-k))  
                | otherwise = 0

main :: IO()
main = print $ combination 40 20
