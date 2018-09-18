-- Consider the following "magic" 3-gon ring, filled with the numbers
-- 1 to 6, and each line adding to nine.
--
--           4
--            \
--             3
--            / \
--           1 - 2 - 6
--          /
--         5
--
-- Working clockwise, and starting from the group of three with the
-- numerically lowest external node (4,3,2 in this example), each
-- solution can be described uniquely. For example, the above solution
-- can be described by the set: 4,3,2; 6,2,1; 5,1,3.
-- 
-- It is possible to complete the ring with four different totals:
--     9, 10, 11, and 12. There are eight solutions in total.
-- 
--          Total	Solution Set
--          9	4,2,3; 5,3,1; 6,1,2
--          9	4,3,2; 6,2,1; 5,1,3
--          10	2,3,5; 4,5,1; 6,1,3
--          10	2,5,3; 6,3,1; 4,1,5
--          11	1,4,6; 3,6,2; 5,2,4
--          11	1,6,4; 5,4,2; 3,2,6
--          12	1,5,6; 2,6,4; 3,4,5
--          12	1,6,5; 3,5,4; 2,4,6
-- 
-- By concatenating each group it is possible to form 9-digit strings; the
-- maximum string for a 3-gon ring is 432621513.
-- 
-- Using the numbers 1 to 10, and depending on arrangements, it is possible
-- to form 16- and 17-digit strings. What is the maximum 16-digit string for a "magic" 5-gon ring?
import Data.List (permutations, elemIndex, nub, sort)
unique lst = nub lst

toInt str = read str :: Int
flatten lst = foldr (++) [] lst
allTheSame lst = and $ zipWith (==) lst (drop 1 lst)

-- (external, circle, circle)
type Node = (Int, Int, Int)
get_first  (a, b, c) = a
get_second (a, b, c) = b
get_third  (a, b, c) = c

nodeToStr (a, b, c) = flatten $ map show [a, b, c]

-- rotate list left n times
rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) $ drop n (cycle xs)

rotateToLowest :: [Node] -> [Node]
rotateToLowest lst = rotate n lst
    where
        l = map get_first lst
        m = foldr min (head l) (tail l)
        n = maybe (-1) id  (elemIndex m l)

nodesToLowestConcatNumber lst = toInt $ flatten $ map nodeToStr $ rotateToLowest lst

listOfIntToNodes lst = map asd [(1,2,3),(4,3,5),(6,5,7),(8,7,9),(10,9,2)]
    where asd (a, b, c) = (lst!!(a-1), lst!!(b-1), lst!!(c-1))

isMagic :: [Node] -> Bool
isMagic lst = (allTheSame $ map s lst) && (elem 10 $ map get_first lst)
    where
        s (a, b, c) = a+b+c

main :: IO()
main = print $ foldr max 0 $ map nodesToLowestConcatNumber $ filter isMagic $ map listOfIntToNodes $ permutations [1..10]
