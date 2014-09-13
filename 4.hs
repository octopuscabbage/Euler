import Data.List

isPalindrome ::  Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome (s:[]) = True
isPalindrome s = (head s) == (last s) && isPalindrome(tail (init s))

combinations ::  [a] -> [a] -> [(a, a)]
combinations xs zs = [(x,z) | x <- xs, z <- zs]

makesPalindrome:: (Eq a, Num a,Show a) => (a,a) -> Bool
makesPalindrome (x,y) = isPalindrome $ show (x * y)

main = print $last $ sort $ map (\x -> fst x * snd x)$ filter makesPalindrome $ combinations [100..999] [100..999]
