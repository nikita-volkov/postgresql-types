module TimeExtras.TimeOfDay where

import Data.Fixed
import Data.Time
import Prelude

fromMicroseconds :: Int -> TimeOfDay
fromMicroseconds microseconds =
  let (minutes, microseconds') = divMod microseconds 60_000_000
      (hours, minutes') = divMod minutes 60
      picoseconds = MkFixed (fromIntegral microseconds' * 1_000_000)
   in TimeOfDay hours minutes' picoseconds

toMicroseconds :: TimeOfDay -> Int
toMicroseconds (TimeOfDay hours minutes picoseconds) =
  let MkFixed picoseconds' = picoseconds
      microseconds = div picoseconds' 1_000_000
      microseconds' = fromIntegral ((hours * 60 + minutes) * 60) * 1_000_000 + fromIntegral microseconds
   in microseconds'
