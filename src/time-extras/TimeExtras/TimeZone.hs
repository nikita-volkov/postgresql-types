module TimeExtras.TimeZone where

import Data.Fixed
import Data.Time
import Prelude

fromSeconds :: Int -> TimeZone
fromSeconds seconds =
  let -- Round to the nearest minute instead of truncating
      minutes = (seconds + 30) `div` 60
   in minutesToTimeZone minutes

toMinutes :: TimeZone -> Int
toMinutes (TimeZone minutes _ _) = minutes

toSeconds :: TimeZone -> Int
toSeconds (TimeZone minutes _ _) = minutes * 60
