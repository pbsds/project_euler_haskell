-- 2520 is the smallest number that can be divided by each of the numbers from 1
-- to 10 without any remainder. What is the smallest positive number that is
-- evenly divisible by all of the numbers from 1 to 20?

doesMatch n = all (\x -> mod n x == 0) [20,19..1]


main :: IO ()
main = print $ take 1 $ filter doesMatch [1..]
