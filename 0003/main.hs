-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?

isqrt n = floor $ sqrt $ fromIntegral n
isPrime n = null [ x | x <- [2..isqrt n], mod n x  == 0]

t = 600851475143

sub_t_primes = filter isPrime $ reverse [1..isqrt t]
t_divisors = filter (\x -> mod t x == 0) sub_t_primes

main :: IO ()
main = print $ take 1 t_divisors
