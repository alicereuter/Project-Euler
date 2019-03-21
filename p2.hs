fibs =  0 : 1 : zipWith (+) fibs (tail fibs)

z = takeWhile (\x -> (x < 4000000) ) fibs

q = [x | x <- z, x `mod` 2 == 0]
