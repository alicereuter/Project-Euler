import qualified Data.Char as Char

--p :: (Fractional a,Integer b) => [b] -> a -> a
p n x = if null n then 0 else (head n)*(x^((length n)-1)) + p (tail n) x

possibleRoots :: (Fractional b) => [Integer] -> [b]
possibleRoots n = foldl1 (++) $ [[(fromIntegral x) /(fromIntegral y), -(fromIntegral x) / (fromIntegral y)] | x <- factors f , y <- factors l]
   where f  = head n
         l = last n

pRoots :: (Fractional b,Enum b) => [b]
pRoots = [-9 .. 0]

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral


factors n = [(fromInteger x) | x <- [1..n],n `rem` x == 0]

hasIroot :: (Fractional a,Enum a) => [Integer] -> [a]
hasIroot n 
  | l == 0     = [0]
  | otherwise  = map (p (map (fromIntegral) n) ) $ [-9 .. 0]
             
             where l = last n

iCanHazRoot n = not $ null $ [x | x <- hasIroot (map toInteger n),  x == 0]

bdig :: Integer -> [Int]
bdig = map Char.digitToInt . show 

-------------------------

isTrue a | a == True = True
         | a == False = False

z  x = length $ filter (True &&) $ map iCanHazRoot (map (bdig) [1 .. x])
