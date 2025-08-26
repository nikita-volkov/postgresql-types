module PrimitiveLayer.Types.Macaddr (Macaddr) where

import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @macaddr@ type. MAC (Media Access Control) address.
--
-- Represents a @6@-byte MAC address, typically used in networking.
-- The format is six groups of two hexadecimal digits, separated by colons.
-- Example: @01:23:45:67:89:ab@
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-net-types.html#DATATYPE-MACADDR).
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
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    e <- arbitrary
    f <- arbitrary
    pure (Macaddr a b c d e f)
  shrink (Macaddr a b c d e f) =
    [ Macaddr a' b' c' d' e' f'
    | a' <- shrink a,
      b' <- shrink b,
      c' <- shrink c,
      d' <- shrink d,
      e' <- shrink e,
      f' <- shrink f
    ]

instance Mapping Macaddr where
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
    PtrPeeker.fixed
      ( Right
          <$> ( Macaddr
                  <$> PtrPeeker.unsignedInt1
                  <*> PtrPeeker.unsignedInt1
                  <*> PtrPeeker.unsignedInt1
                  <*> PtrPeeker.unsignedInt1
                  <*> PtrPeeker.unsignedInt1
                  <*> PtrPeeker.unsignedInt1
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
  onfrom (a, b, c, d, e, f) = Macaddr a b c d e f

-- | Direct conversion from Macaddr to 6-tuple of Word8.
-- This is a total conversion as it always succeeds.
instance IsMany Macaddr (Word8, Word8, Word8, Word8, Word8, Word8) where
  onfrom (Macaddr a b c d e f) = (a, b, c, d, e, f)

-- | Bidirectional conversion between 6-tuple of Word8 and Macaddr.
instance Is (Word8, Word8, Word8, Word8, Word8, Word8) Macaddr

instance Is Macaddr (Word8, Word8, Word8, Word8, Word8, Word8)
