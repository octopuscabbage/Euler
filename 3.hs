import Data.List

getFactors n = getPair $ findFirstFactor n
	where getPair x = [x,(n`div`x)]

findFirstFactor a = findFirstFactorSeeded a [2..((a`div`2)+1)]

findFirstFactorSeeded 1 _ = 1
findFirstFactorSeeded a (x:xs) 
	| isPrime a = 1
	| (a /? x) =  x
	| otherwise = findFirstFactorSeeded a xs	

isPrime a = not (isEven a || isDisvisibleByOddsUpToSqrt a) || a ==2 || a ==1 
	where	isEven n = n /? 2
		isDisvisibleByOddsUpToSqrt x = any (\n -> x/?n) [3,5..x`div`2] 

eulerNumber = 600851475143

a /? b = a `mod` b == 0

bindFAndKeepUnique f xs = nub ( xs >>= f )

main = print $ head $ reverse $ sort $ until (all isPrime) (bindFAndKeepUnique getFactors) [eulerNumber]

	

