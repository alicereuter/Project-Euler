import Data.List.Split (splitOn)
import Data.List (isInfixOf)
import Data.Char (ord,chr)

keys = [[[x],[y],[z]] | x <- ['a' .. 'z'], y <- ['a' .. 'z'], z <- ['a' .. 'z']]

transfer x key = do
  let f = take 3202 x
  let z = splitOn "," f
  let h = map (\x -> read x :: Int) z
  let q = map toBin h
  let key1 = map (\x -> ord (x !! 0)) key
  let infkey = cycle $ map toBin key1
  let i = zip q infkey
  let res = map (\x -> zip (fst x ) (snd x)) i
  let a   = map (\x -> map (\z -> xor (fst z) (snd z)) x) res
  let a1  = map fromBin a
  let z   = map chr a1
  return z

main = do
  let key = ["g","o","d"]
  x <- readFile "p059_cipher.txt"
  f <- (transfer x) key
  --let h = [x | x <- f, isInfixOf "the " x]
  return f
  
 
  
xor 1 0 = 1
xor 0 1 = 1
xor _ _ = 0


fromBin :: [Int] -> Int
fromBin y
  | y == []   = 0
  | otherwise = x*2^(length xs) + fromBin xs
  where x = head y
        xs = tail y

toBin :: Int -> [Int]
toBin x = if l < 8
  then (take (8-l) (cycle [0])) ++  conv
  else  conv
  where conv = toBinary x
        l    = length conv

toBinary :: Int -> [Int]
toBinary x
  | x == 0 = []
  | otherwise =( toBinary res) ++ [bit]
  where res  = (floor ((fromIntegral x)/ (fromIntegral 2)))
        bit  = x `mod` 2


