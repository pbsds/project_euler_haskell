-- 21^5 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
-- What is the sum of the digits of the number 2^1000?

digitToInt c = maybe 0 id $ lookup c (zip ['0'..'9'] [0..9])

main :: IO()
main = print $ sum $ map digitToInt $ show (2^1000)
