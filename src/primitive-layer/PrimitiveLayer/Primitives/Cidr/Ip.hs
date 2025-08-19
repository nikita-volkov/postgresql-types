module PrimitiveLayer.Primitives.Cidr.Ip
  ( CidrIp (..),
    maskedV4,
    maskedV6,
  )
where

import Data.Bits
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import GHC.Records
import Numeric (showHex)
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Vias
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | IP address type for representing IPv4 and IPv6 addresses.
data CidrIp
  = -- | IPv4 address stored as 32-bit big-endian word
    V4CidrIp Word32
  | -- | IPv6 address stored as four 32-bit big-endian words
    V6CidrIp Word32 Word32 Word32 Word32
  deriving stock (Eq, Ord, Show)

instance Bounded CidrIp where
  minBound = V4CidrIp 0
  maxBound = V6CidrIp maxBound maxBound maxBound maxBound

instance Arbitrary CidrIp where
  arbitrary = do
    isIPv4 <- arbitrary
    if isIPv4
      then V4CidrIp <$> arbitrary
      else V6CidrIp <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (V4CidrIp w) = V4CidrIp <$> shrink w
  shrink (V6CidrIp w1 w2 w3 w4) =
    [V6CidrIp w1' w2' w3' w4' | (w1', w2', w3', w4') <- shrink (w1, w2, w3, w4)]

applyNetmask :: Word8 -> CidrIp -> CidrIp
applyNetmask netmask address =
  case address of
    V4CidrIp w ->
      maskedV4 (min 32 (fromIntegral netmask)) w
    V6CidrIp w1 w2 w3 w4 ->
      maskedV6 (min 128 (fromIntegral netmask)) w1 w2 w3 w4

maskedV4 :: Int -> Word32 -> CidrIp
maskedV4 netmask w =
  let hostBits = 32 - netmask
      -- Create mask with network bits as 1, host bits as 0
      networkMask = if hostBits >= 32 then 0 else complement ((1 `shiftL` hostBits) - 1)
      normalizedAddr = w .&. networkMask
   in V4CidrIp normalizedAddr

maskedV6 :: Int -> Word32 -> Word32 -> Word32 -> Word32 -> CidrIp
maskedV6 netmask w1 w2 w3 w4
  | netmask <= 0 = V6CidrIp 0 0 0 0 -- No network bits to preserve
  | netmask >= 128 = V6CidrIp w1 w2 w3 w4 -- All bits are network bits
  | netmask >= 96 = V6CidrIp w1 w2 w3 (mask (netmask - 96) w4)
  | netmask >= 64 = V6CidrIp w1 w2 (mask (netmask - 64) w3) 0
  | netmask >= 32 = V6CidrIp w1 (mask (netmask - 32) w2) 0 0
  | otherwise = V6CidrIp (mask netmask w1) 0 0 0
  where
    mask a b =
      if a <= 0
        then 0
        else
          if a >= 32
            then b
            else b .&. complement ((1 `shiftL` (32 - a)) - 1)
