
numbers = filter (\x -> x`mod`3==0 || x`mod`5==0) [1..999]

main = 	print $ foldl1 (+) numbers
