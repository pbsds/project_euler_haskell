-- If the numbers 1 to 5 are written out in words: one, two, three, four, five,
-- then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
-- 
-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out
-- in words, how many letters would be used?
-- 
-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
-- forty-two) contains 23 letters and 115 (one hundred and fifteen) contains
-- 20 letters. The use of "and" when writing out numbers is in compliance
-- with British usage.

numberLength  0 = 0
numberLength  1 = 3 -- one
numberLength  2 = 3 -- two
numberLength  3 = 5 -- three
numberLength  4 = 4 -- four
numberLength  5 = 4 -- five
numberLength  6 = 3 -- six
numberLength  7 = 5 -- seven
numberLength  8 = 5 -- eight
numberLength  9 = 4 -- nine
numberLength 10 = 3 -- ten
numberLength 11 = 6 -- eleven
numberLength 12 = 6 -- twelve
numberLength 13 = 8 -- thirteen
numberLength 14 = 8 -- fourteen
numberLength 15 = 7 -- fiftheen
numberLength 16 = 7 -- sixteen
numberLength 17 = 9 -- seventeen
numberLength 18 = 8 -- eighteen
numberLength 19 = 8 -- nineteen
numberLength 20 = 6 -- twenty
numberLength 30 = 6 -- thirty
numberLength 40 = 5 -- forty
numberLength 50 = 5 -- fifty
numberLength 60 = 5 -- sixty
numberLength 70 = 7 -- seventy
numberLength 80 = 6 -- eighty
numberLength 90 = 6 -- ninety
numberLength 1000 = 11 -- one thousand

numberLength n | n >= 100  = numberLength (div n 100) + 7 + if (mod n 100 /= 0) then 3 + numberLength (mod n 100) else 0
               | otherwise = numberLength ((div n 10)*10) + numberLength (mod n 10)

main :: IO ()
main = print $ sum $ map numberLength [1..1000]
