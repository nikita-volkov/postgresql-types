module TimeExtras.TimeOfDay where

import Control.Monad
import Data.Fixed
import Data.Time
import Prelude

convertFromMicroseconds :: Int -> TimeOfDay
convertFromMicroseconds microseconds =
  let (minutes, microseconds') = divMod microseconds 60_000_000
      (hours, minutes') = divMod minutes 60
      picoseconds = MkFixed (fromIntegral microseconds' * 1_000_000)
   in TimeOfDay hours minutes' picoseconds

normalizeToMicroseconds :: TimeOfDay -> Int
normalizeToMicroseconds (TimeOfDay hours minutes picoseconds) =
  let MkFixed picoseconds' = picoseconds
      microseconds = div picoseconds' 1_000_000
      microseconds' = fromIntegral ((hours * 60 + minutes) * 60) * 1_000_000 + fromIntegral microseconds
   in microseconds'

projectToMicroseconds :: TimeOfDay -> Maybe Int
projectToMicroseconds (TimeOfDay hours minutes picoseconds) = do
  let MkFixed picoseconds' = picoseconds
      (microseconds, remainder) = divMod picoseconds' 1_000_000
  guard (remainder == 0)
  let microseconds' = fromIntegral ((hours * 60 + minutes) * 60) * 1_000_000 + fromIntegral microseconds
  pure microseconds'
