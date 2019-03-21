import Data.Numbers.Primes
import Data.Set
z = [p + 2*n |  n <- [1..],p <- take 100 primes]
n = [0 ..]
a = toList $ difference (fromList z) (fromList n)
f =  [x | x <- a , (not (isPrime x)) && x `mod` 2 == 1]
