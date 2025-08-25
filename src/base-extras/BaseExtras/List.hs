module BaseExtras.List where

import Prelude

toPairs :: [a] -> [(a, a)]
toPairs [] = []
toPairs [x] = []
toPairs (x : y : xs) = (x, y) : toPairs xs
