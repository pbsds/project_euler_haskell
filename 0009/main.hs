-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
--     a^2 + b^2 = c^2
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.


isTarget a b = a^2 + b ^2 == (1000-a-b)^2

main :: IO ()
main = print $ take 1 [x * y * (1000-x-y) | x<-[1..], y<-[1..x-1], isTarget x y]
