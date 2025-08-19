module PrimitiveLayer.Vias.IsMany where

import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude

newtype ViaIsMany a b = ViaIsMany b
  deriving newtype (Eq, Ord, Arbitrary, Bounded)

instance IsSome b (ViaIsMany a b) where
  to = coerce

instance IsSome (ViaIsMany a b) b where
  to = ViaIsMany

instance IsMany b (ViaIsMany a b)

instance IsMany (ViaIsMany a b) b

instance Is b (ViaIsMany a b)

instance Is (ViaIsMany a b) b

instance (IsMany a b) => IsSome a (ViaIsMany a b) where
  to = to @a . to @b
  maybeFrom = fmap ViaIsMany . maybeFrom @a

instance (IsMany a b) => IsMany a (ViaIsMany a b) where
  from = ViaIsMany . from

instance (Primitive a, IsMany a b) => Primitive (ViaIsMany a b) where
  typeName = retag @a typeName
  baseOid = retag @a baseOid
  arrayOid = retag @a arrayOid
  binaryEncoder = binaryEncoder . to @a
  binaryDecoder = fmap (fmap (from @a)) binaryDecoder
  textualEncoder = textualEncoder . to @a
