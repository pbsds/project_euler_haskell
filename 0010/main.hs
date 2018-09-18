-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
-- Find the sum of all the primes below two million.

import Data.Numbers.Primes

main :: IO ()
main = print $ sum $ takeWhile (2000000>) primes
