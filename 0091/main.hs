-- The points P (x1, y1) and Q (x2, y2) are plotted at integer
-- co-ordinates and are joined to the origin, O(0,0), to form ΔOPQ.
-- 
-- There are exactly fourteen triangles containing a right angle
-- that can be formed when each co-ordinate lies between 0 and 2 inclusive;
-- that is, 0 ≤ x1, y1, x2, y2 ≤ 2.
-- 
-- Given that 0 ≤ x1, y1, x2, y2 ≤ 50, how many right triangles can be formed?

isValid (a, b, c) = a/=b && a/=c && b/=c

isRightAngled (a, b, c) = l1==l2+l3 || l2==l1+l3 || l3==l1+l2
    where
        l (x1, y1) (x2, y2) = (x2-x1)^2 + (y2-y1)^2
        l1 = l b a
        l2 = l c a
        l3 = l b c

size = 50

points = [(x, y) | x<-[0..size], y<-[0..size]]
triangles = filter isRightAngled $ filter isValid [ ((0, 0), a, b) |a<-points, b<-points]

main :: IO()
main = print $ div (length triangles) 2
