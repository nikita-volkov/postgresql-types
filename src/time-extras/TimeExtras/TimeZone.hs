module TimeExtras.TimeZone where

import Data.Fixed
import Data.Time
import Prelude

convertFromMinutes :: Int -> TimeZone
convertFromMinutes = minutesToTimeZone

-- |
-- Normalize, canonicalize, compress.
compressFromSeconds :: Int -> TimeZone
compressFromSeconds seconds =
  let minutes = seconds `div` 60
   in convertFromMinutes minutes

-- |
-- Compile, distill, rectify seconds. Extract from seconds.
compileFromSeconds :: Int -> Maybe TimeZone
compileFromSeconds seconds =
  let (minutes, remainder) = seconds `divMod` 60
   in if remainder == 0
        then Just (convertFromMinutes minutes)
        else Nothing

convertToMinutes :: TimeZone -> Int
convertToMinutes (TimeZone minutes _ _) = minutes

-- | Dilute.
convertToSeconds :: TimeZone -> Int
convertToSeconds (TimeZone minutes _ _) = minutes * 60
