
mapReduce mapper reducer xs = foldl1 reducer $ map mapper xs

main = print $ ((foldl1 (+) [1..100])^2) - (mapReduce (^2) (+) [1..100])


