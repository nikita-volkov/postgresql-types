module PrimitiveLayer.Primitives.Macaddr8 (Macaddr8 (..)) where

import Control.Monad (replicateM)
import qualified Data.Text as Text
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Vias
import qualified PtrPoker.Write as Write
import Text.Printf (printf)
import qualified TextBuilder

-- | @macaddr8@. MAC (Media Access Control) address (EUI-64 format).
--
-- PostgreSQL @macaddr8@ type representing an @8@-byte MAC address.
-- This is used for EUI-64 format MAC addresses.
-- The format is eight groups of two hexadecimal digits, separated by colons.
-- Example: @01:23:45:67:89:ab:cd:ef@
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-net-types.html#DATATYPE-MACADDR8)
data Macaddr8 = Macaddr8
  { macaddr8Byte1 :: Word8,
    macaddr8Byte2 :: Word8,
    macaddr8Byte3 :: Word8,
    macaddr8Byte4 :: Word8,
    macaddr8Byte5 :: Word8,
    macaddr8Byte6 :: Word8,
    macaddr8Byte7 :: Word8,
    macaddr8Byte8 :: Word8
  }
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaPrimitive Macaddr8)

instance Arbitrary Macaddr8 where
  arbitrary = do
    bytes <- replicateM 8 arbitrary
    -- Ensure not all bytes are zero as that might be invalid
    if all (== 0) bytes
      then pure (Macaddr8 0 0 0 0 0 0 0 1) -- Use a valid non-zero MAC
      else case bytes of
        [a, b, c, d, e, f, g, h] -> pure (Macaddr8 a b c d e f g h)
        _ -> error "impossible case"
  shrink (Macaddr8 a b c d e f g h) =
    [ Macaddr8 a' b' c' d' e' f' g' h'
    | (a', b', c', d', e', f', g', h') <- shrink (a, b, c, d, e, f, g, h),
      not (a' == 0 && b' == 0 && c' == 0 && d' == 0 && e' == 0 && f' == 0 && g' == 0 && h' == 0)
    ]

instance Primitive Macaddr8 where
  typeName = Tagged "macaddr8"
  baseOid = Tagged 774
  arrayOid = Tagged 775
  binaryEncoder (Macaddr8 a b c d e f g h) =
    mconcat
      [ Write.word8 a,
        Write.word8 b,
        Write.word8 c,
        Write.word8 d,
        Write.word8 e,
        Write.word8 f,
        Write.word8 g,
        Write.word8 h
      ]
  binaryDecoder =
    PeekyBlinders.statically
      ( Right
          <$> ( Macaddr8
                  <$> PeekyBlinders.unsignedInt1
                  <*> PeekyBlinders.unsignedInt1
                  <*> PeekyBlinders.unsignedInt1
                  <*> PeekyBlinders.unsignedInt1
                  <*> PeekyBlinders.unsignedInt1
                  <*> PeekyBlinders.unsignedInt1
                  <*> PeekyBlinders.unsignedInt1
                  <*> PeekyBlinders.unsignedInt1
              )
      )
  textualEncoder (Macaddr8 a b c d e f g h) =
    TextBuilder.intercalate ":"
      $ [ formatByte a,
          formatByte b,
          formatByte c,
          formatByte d,
          formatByte e,
          formatByte f,
          formatByte g,
          formatByte h
        ]
    where
      formatByte :: Word8 -> TextBuilder.TextBuilder
      formatByte x = TextBuilder.string (printf "%02x" x)

-- | Convert from a tuple of 8 Word8s to Macaddr8.
instance IsSome (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8) Macaddr8 where
  to (Macaddr8 a b c d e f g h) = (a, b, c, d, e, f, g, h)
  maybeFrom (a, b, c, d, e, f, g, h) = Just (Macaddr8 a b c d e f g h)

-- | Convert from Macaddr8 to a tuple of 8 Word8s.
instance IsSome Macaddr8 (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8) where
  to (a, b, c, d, e, f, g, h) = Macaddr8 a b c d e f g h
  maybeFrom (Macaddr8 a b c d e f g h) = Just (a, b, c, d, e, f, g, h)

-- | Convert from a tuple of 8 Word8s to Macaddr8.
instance IsMany (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8) Macaddr8 where
  from (a, b, c, d, e, f, g, h) = Macaddr8 a b c d e f g h

-- | Convert from Macaddr8 to a tuple of 8 Word8s.
instance IsMany Macaddr8 (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8) where
  from (Macaddr8 a b c d e f g h) = (a, b, c, d, e, f, g, h)

-- | Bidirectional conversion between tuple and Macaddr8.
instance Is (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8) Macaddr8

instance Is Macaddr8 (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
