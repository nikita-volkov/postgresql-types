module TimeExtras.TimeZone where

import Data.Time
import Prelude

convertFromMinutes :: Int -> TimeZone
convertFromMinutes = minutesToTimeZone

-- |
-- Normalize, canonicalize, compress.
compressFromSeconds :: Int -> TimeZone
compressFromSeconds seconds =
  if seconds < 0
    then fromSignerAndAbsSeconds negate (negate seconds)
    else fromSignerAndAbsSeconds id seconds
  where
    fromSignerAndAbsSeconds signer seconds =
      let minutes = signer (div seconds 60)
       in convertFromMinutes minutes

-- |
-- Compile, distill, rectify seconds. Extract from seconds.
projectFromSeconds :: Int -> Maybe TimeZone
projectFromSeconds seconds =
  if seconds < 0
    then fromSignerAndAbsSeconds negate (negate seconds)
    else fromSignerAndAbsSeconds id seconds
  where
    fromSignerAndAbsSeconds signer seconds =
      let (minutes, remainder) = divMod seconds 60
       in if remainder == 0
            then Just (convertFromMinutes (signer minutes))
            else Nothing

convertToMinutes :: TimeZone -> Int
convertToMinutes (TimeZone minutes _ _) = minutes

-- | Dilute.
convertToSeconds :: TimeZone -> Int
convertToSeconds (TimeZone minutes _ _) = minutes * 60
