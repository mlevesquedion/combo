module Combo where

factorial :: (Integral a) => a -> a
factorial n
  | n <= 0 = 1
  | otherwise = product [1..n]

choose :: Integral a => a -> a -> a
choose n k = div num den
  where num = factorial n
        den = factorial (n - k) * factorial k
