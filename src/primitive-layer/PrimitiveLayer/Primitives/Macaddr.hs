module PrimitiveLayer.Primitives.Macaddr where

import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Vias
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | A MAC address type.
-- Represents a 6-byte MAC address, typically used in networking.
-- The format is six groups of two hexadecimal digits, separated by colons.
-- Example: "01:23:45:67:89:ab"
data Macaddr
  = Macaddr
      Word8
      Word8
      Word8
      Word8
      Word8
      Word8
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaPrimitive Macaddr)

instance Arbitrary Macaddr where
  arbitrary = do
    a <- arbitrary @Word8
    b <- arbitrary @Word8
    c <- arbitrary @Word8
    d <- arbitrary @Word8
    e <- arbitrary @Word8
    f <- arbitrary @Word8
    -- Use from to ensure consistency with IsMany instance
    pure $ from (a, b, c, d, e, f)
  shrink (Macaddr a b c d e f) =
    [ from (a', b', c', d', e', f')
    | (a', b', c', d', e', f') <- shrink (a, b, c, d, e, f)
    ]

instance Primitive Macaddr where
  typeName = Tagged "macaddr"
  baseOid = Tagged 829
  arrayOid = Tagged 1040
  binaryEncoder (Macaddr a b c d e f) =
    mconcat
      [ Write.word8 a,
        Write.word8 b,
        Write.word8 c,
        Write.word8 d,
        Write.word8 e,
        Write.word8 f
      ]
  binaryDecoder =
    PeekyBlinders.statically
      ( Right
          <$> ( Macaddr
                  <$> PeekyBlinders.unsignedInt1
                  <*> PeekyBlinders.unsignedInt1
                  <*> PeekyBlinders.unsignedInt1
                  <*> PeekyBlinders.unsignedInt1
                  <*> PeekyBlinders.unsignedInt1
                  <*> PeekyBlinders.unsignedInt1
              )
      )
  textualEncoder (Macaddr a b c d e f) =
    (TextBuilder.intercalate ":")
      [ TextBuilder.hexadecimal a,
        TextBuilder.hexadecimal b,
        TextBuilder.hexadecimal c,
        TextBuilder.hexadecimal d,
        TextBuilder.hexadecimal e,
        TextBuilder.hexadecimal f
      ]

-- | Direct conversion from 6-tuple of Word8 to Macaddr.
-- This is always safe since both represent the same MAC address.
instance IsSome (Word8, Word8, Word8, Word8, Word8, Word8) Macaddr where
  to (Macaddr a b c d e f) = (a, b, c, d, e, f)
  maybeFrom (a, b, c, d, e, f) = Just (Macaddr a b c d e f)

-- | Direct conversion from Macaddr to 6-tuple of Word8.
-- This is always safe since both represent the same MAC address.
instance IsSome Macaddr (Word8, Word8, Word8, Word8, Word8, Word8) where
  to (a, b, c, d, e, f) = Macaddr a b c d e f
  maybeFrom (Macaddr a b c d e f) = Just (a, b, c, d, e, f)

-- | Direct conversion from 6-tuple of Word8 to Macaddr.
-- This is a total conversion as it always succeeds.
instance IsMany (Word8, Word8, Word8, Word8, Word8, Word8) Macaddr where
  from (a, b, c, d, e, f) = Macaddr a b c d e f

-- | Direct conversion from Macaddr to 6-tuple of Word8.
-- This is a total conversion as it always succeeds.
instance IsMany Macaddr (Word8, Word8, Word8, Word8, Word8, Word8) where
  from (Macaddr a b c d e f) = (a, b, c, d, e, f)

-- | Bidirectional conversion between 6-tuple of Word8 and Macaddr.
instance Is (Word8, Word8, Word8, Word8, Word8, Word8) Macaddr

instance Is Macaddr (Word8, Word8, Word8, Word8, Word8, Word8)
