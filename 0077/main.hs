-- It is possible to write ten as the sum of primes in exactly five
-- different ways:
-- 
-- 7 + 3
-- 5 + 5
-- 5 + 3 + 2
-- 3 + 3 + 2 + 2
-- 2 + 2 + 2 + 2 + 2
-- 
-- What is the first value which can be written as the sum of
-- primes in over five thousand different ways?


-- from https://www.reddit.com/r/haskell/comments/35vc31/the_real_way_to_generate_a_list_of_primes_in/
primes :: [Int]
primes = 2 : 3 : 5 : primes'
    where
        isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
        primes' = 7 : filter (isPrime primes') (scanl (+) 11 $ cycle [2,4,2,4,6,2,6,4])

partition _ 0 = 1
partition prim@(p:ps) n = if n >= p
    then partition prim (n-p) + partition ps n
    else 0

main :: IO()
main = print $ head $ dropWhile ((5000>) . partition primes) [1..]
