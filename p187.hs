import Data.Numbers.Primes

z = takeWhile (<10^8) primes

helf = takeWhile (<10^4) primes
sr   = map (^2) helf
