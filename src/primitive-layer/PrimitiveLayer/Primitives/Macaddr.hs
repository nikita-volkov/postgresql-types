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
