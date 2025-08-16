module PrimitiveLayer.Primitives.Interval.Micros where

import PrimitiveLayer.Prelude hiding (max, min)

min :: Integer
min = negate max

max :: Integer
max = 1780000 * year

year :: Integer
year = round rational
  where
    rational :: Rational
    rational = 365.25 * fromIntegral day

month :: Integer
month = 30 * day

day :: Integer
day = 10 ^ 6 * 60 * 60 * 24
