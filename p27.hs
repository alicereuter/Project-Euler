import Data.Numbers.Primes
b = takeWhile (<1000) primes

a = [-1000 .. 1000]

out a b x = x^2+a*x+b

z = [ ((out x y), (x,y))| x <- a, y <- b]

q = map (\x -> (length $ takeWhile (isPrime)  (map (fst x) [0..]),    snd x )) z 
f = [x | x <- q, (fst x) > 70]
