-- By using each of the digits from the set, {1, 2, 3, 4}, exactly once,
-- and making use of the four arithmetic operations (+, −, *, /) and
-- brackets/parentheses, it is possible to form different positive
-- integer targets. For example,
-- 
--       8 = (4 * (1 + 3)) / 2
--       14 = 4 * (3 + 1 / 2)
--       19 = 4 * (2 + 3) − 1
--       36 = 3 * 4 * (2 + 1)
-- 
-- Note that concatenations of the digits, like 12 + 34, are not allowed.
-- Using the set, {1, 2, 3, 4}, it is possible to obtain thirty-one different
-- target numbers of which 36 is the maximum, and each of the numbers 1 to 28
-- can be obtained before encountering the first non-expressible number.
-- Find the set of four distinct digits, a < b < c < d, for which the longest
-- set of consecutive positive integers, 1 to n, can be obtained, giving your
-- answer as a string: abcd.
import Data.List
unique lst = nub lst

--flattens a list of listss
flatten [] = []
flatten (x:xs) = x++flatten xs

isWhole n = fromIntegral (round n) == n

--produces all ways to split a list into two pairs
splits set = filter asd $ zipWith (\a b -> (a\\b, b)) (repeat set) (subsequences set)
    where asd (a, b) = not (null a) && not (null b)


--combine all arithmetic
combine lst | length lst >= 2 = unique $ flatten $ map asd $ splits lst
    where
        asd (a, b) = [x+y|x<-a',y<-b']++[x-y|x<-a',y<-b']++[x*y|x<-a',y<-b']++[x/y|x<-a',y<-b']
            where
                a' = combine a
                b' = combine b
combine lst | length lst == 1 = lst
combine lst | length lst == 2 = unique [a+b, a*b, a-b, b-a, a/b, b/a]
    where
        a = lst !! 0
        b = lst !! 1


--consecutive positive integers max
consecLen lst = asd lst 1
    where
        asd [] n = n-1
        asd (x:xs) n = if n == x then asd xs (n+1) else n-1


main :: IO ()
main = print $ numbersetToString $ snd $ foldr max' (-1, []) $ zip (map caclResult numberSets) numberSets
    where
        numbersetToString set = flatten $ map show $ map round set
        max' (val, set) (val2, set2) = if val > val2 then (val, set) else (val2, set2)
        numberSets = unique $ map sort $ map (take 4) $ permutations [1..9]
        caclResult numbers = consecLen $ sort $ map round $ filter (0<) $ filter isWhole $ combine numbers
