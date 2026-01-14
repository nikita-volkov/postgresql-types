module PostgresqlTypes.Via.IsSome where

import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude hiding (ViaIsSome)
import Test.QuickCheck as Qc

-- |
-- Helper for deriving instances based on the 'IsSome' projection onto a superset type.
newtype ViaIsSome a b = ViaIsSome b
  deriving newtype (Eq, Ord)

instance IsSome b (ViaIsSome a b) where
  to = coerce

instance IsSome (ViaIsSome a b) b where
  to = ViaIsSome

instance IsMany b (ViaIsSome a b)

instance IsMany (ViaIsSome a b) b

instance Is b (ViaIsSome a b)

instance Is (ViaIsSome a b) b

instance (IsSome a b) => IsSome a (ViaIsSome a b) where
  to = to @a . to @b
  maybeFrom = fmap ViaIsSome . maybeFrom @a

instance (IsStandardType a, IsSome a b) => IsStandardType (ViaIsSome a b) where
  typeName = retag @a typeName
  baseOid = retag @a baseOid
  arrayOid = retag @a arrayOid
  binaryEncoder = binaryEncoder . to @a
  binaryDecoder =
    binaryDecoder <&> \result -> do
      value <- result
      tryFrom (errByValue value) value
    where
      errByValue :: a -> DecodingError
      errByValue value =
        DecodingError
          { location = ["ViaIsSome"],
            reason =
              UnsupportedValueDecodingErrorReason
                ""
                (to @Text (textualEncoder value))
          }
  textualEncoder = textualEncoder . to @a

instance (Arbitrary a, IsSome a b) => Arbitrary (ViaIsSome a b) where
  arbitrary =
    Qc.suchThatMap
      (arbitrary @a)
      (fmap ViaIsSome . maybeFrom)
  shrink (ViaIsSome b) =
    mapMaybe
      (fmap ViaIsSome . maybeFrom)
      (shrink (to @a b))

instance (Show a, IsSome a b) => Show (ViaIsSome a b) where
  showsPrec p (ViaIsSome b) = showsPrec p (to @a b)
