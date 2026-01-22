module TimeExtras.TimeZone where

import Data.Time
import Prelude

fromMinutes :: Int -> TimeZone
fromMinutes = minutesToTimeZone

-- |
-- Normalize, canonicalize, compress.
normalizeFromSeconds :: Int -> TimeZone
normalizeFromSeconds seconds =
  if seconds < 0
    then fromSignerAndAbsSeconds negate (negate seconds)
    else fromSignerAndAbsSeconds id seconds
  where
    fromSignerAndAbsSeconds signer seconds =
      let minutes = signer (div seconds 60)
       in fromMinutes minutes

-- |
-- Compile, distill, rectify seconds. Extract from seconds.
refineFromSeconds :: Int -> Maybe TimeZone
refineFromSeconds seconds =
  if seconds < 0
    then fromSignerAndAbsSeconds negate (negate seconds)
    else fromSignerAndAbsSeconds id seconds
  where
    fromSignerAndAbsSeconds signer seconds =
      let (minutes, remainder) = divMod seconds 60
       in if remainder == 0
            then Just (fromMinutes (signer minutes))
            else Nothing

toMinutes :: TimeZone -> Int
toMinutes (TimeZone minutes _ _) = minutes

-- | Dilute.
toSeconds :: TimeZone -> Int
toSeconds (TimeZone minutes _ _) = minutes * 60
