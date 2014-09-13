import System.TimeIt



fibo n = seedfibo 1 2 n

seedfibo::Integral a => a-> a-> a -> [a]
seedfibo a b max = 
	if (a > max) then []
		else a : seedfibo b (a+b) max

fourMillion = 4000000

isEven:: Integral a => a -> Bool
isEven x = (x`mod`2==0)

main = timeIt $ print $ foldl1 (+)$ filter isEven $fibo fourMillion
