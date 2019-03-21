
import Data.Numbers.Primes
import Data.Set
factors n = [(fromInteger x) | x <- [1..n],n `rem` x == 0]


isAntiChain x
  | length x == 2 = 1
  | length x == 1 = 1
  | otherwise = length x

isChain z = do
  let x = Prelude.map (\x -> head (factors x)) z
  let f = Prelude.map fromList x
  let z = foldl1 (intersection) f
  return z
import Data.Numbers.Primes
import Data.Set
factors n = [(fromInteger x) | x <- [1..n],n `rem` x == 0]


isAntiChain x
  | length x == 2 = 1
  | length x == 1 = 1
  | otherwise = length x

isChain z = do
  let x = Prelude.map (\x -> head (factors x)) z
  let f = Prelude.map fromList x
  let z = foldl1 (intersection) f
  return z
