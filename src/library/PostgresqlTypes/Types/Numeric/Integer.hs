-- | Utilities for Integer.
module PostgresqlTypes.Types.Numeric.Integer where

import PostgresqlTypes.Prelude

-- | Count the number of digits in an integer (more efficiently)
countDigits :: Integer -> Int
countDigits 0 = 1
countDigits n = go (abs n) 0
  where
    go 0 acc = acc
    go x acc = go (x `div` 10) (acc + 1)
