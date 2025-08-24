module PrimitiveLayer.Via.IsMany where

import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude

-- |
-- Helper for deriving instances based on the 'IsMany' projection onto a superset type.
newtype ViaIsMany a b = ViaIsMany b
  deriving newtype (Eq, Ord)

instance IsSome b (ViaIsMany a b) where
  to = coerce

instance IsSome (ViaIsMany a b) b where
  to = ViaIsMany

instance IsMany b (ViaIsMany a b)

instance IsMany (ViaIsMany a b) b

instance Is b (ViaIsMany a b)

instance Is (ViaIsMany a b) b

instance (IsSome a b) => IsSome a (ViaIsMany a b) where
  to = to @a . to @b
  maybeFrom = fmap ViaIsMany . maybeFrom @a

instance (IsMany a b) => IsMany a (ViaIsMany a b) where
  onfrom = ViaIsMany . onfrom

instance (Mapping a, IsMany a b) => Mapping (ViaIsMany a b) where
  typeName = retag @a typeName
  baseOid = retag @a baseOid
  arrayOid = retag @a arrayOid
  binaryEncoder = binaryEncoder . to @a
  binaryDecoder = fmap (fmap (onfrom @a)) binaryDecoder
  textualEncoder = textualEncoder . to @a

instance (Arbitrary a, IsMany a b) => Arbitrary (ViaIsMany a b) where
  arbitrary = onfrom <$> arbitrary @a
  shrink (ViaIsMany b) = onfrom <$> shrink (to @a b)

instance (Bounded a, IsMany a b) => Bounded (ViaIsMany a b) where
  minBound = onfrom @a minBound
  maxBound = onfrom @a maxBound

instance (Show a, IsMany a b) => Show (ViaIsMany a b) where
  showsPrec p (ViaIsMany b) = showsPrec p (to @a b)
