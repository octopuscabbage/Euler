listPrimes:: [Integral Int]
listPrimes = listPrimesSeeded 2 0

listPrimesSeeded last addend
	| isPrime $ (last + addend) = (last+addend):listPrimesSeeded (last+addend) 1
	| otherwise = listPrimesSeeded last addend+1

isPrime a = not (isEven a || isDisvisibleByOddsUpToSqrt a) || a ==2 || a ==1
        where   isEven n = n /? 2
                isDisvisibleByOddsUpToSqrt x = any (\n -> x/?n) [3,5..x`div`2]

a /? b = a`mod`b==0

True ? x = const x
False ? _ = id

--main = print $ take 10 listPrimes

