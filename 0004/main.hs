-- A palindromic number reads the same both ways. The largest palindrome made
-- from the product of two 2-digit numbers is 9009 = 91 × 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.

digits = [100..999]

isPalindrome n = s == reverse s
    where s = show n

main :: IO ()
main = print $ foldr max 0 [x*y | x <- digits, y <- digits, isPalindrome (x*y)]
