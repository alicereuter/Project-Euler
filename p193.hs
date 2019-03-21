import Data.Numbers.Primes

sqr x = x * x

prime = takeWhile (<2^25) primes


sqrs = map (^2) prime


lessThan x = 2^50/(fromInteger x)
four = 4

--
shiz :: Fractional a => [a]
shiz = [lessThan x| x <- sqrs]

--howMany x = (2^50/(fromIntegral x))

--z = sum $ map (\x -> howMany x) sqrs

factors n = [(fromInteger x) | x <- [1..n], n `rem` x == 0]
shizniz = [factors x | x <- [0 .. 2^50]]

--isDouble :: [a] -> Bool
isDouble x
  | null x = False
  | null (tail x)  = False
  | (x !! 0) == (x !! 1) = True
  | otherwise = isDouble $ tail x

shi = isDouble . prime_factors

prime_factors :: Int -> [Int]
prime_factors 1 = []
prime_factors n
  | factors == []  = [n]
  | otherwise = factors ++ prime_factors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]
