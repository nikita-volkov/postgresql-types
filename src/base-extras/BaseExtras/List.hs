module BaseExtras.List where

toPairs :: [a] -> [(a, a)]
toPairs [] = []
toPairs [_] = []
toPairs (x : y : xs) = (x, y) : toPairs xs
