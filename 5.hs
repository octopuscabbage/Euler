import System.TimeIt

evenlyDivisible s xs = all (s/?) xs

a /? b = a `mod`b ==0

main = timeIt $ print $ until (\s -> evenlyDivisible s [1..20]) (+2) 2
