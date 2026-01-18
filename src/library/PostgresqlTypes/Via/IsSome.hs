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

instance (IsScalar a, IsSome a b) => IsScalar (ViaIsSome a b) where
  typeName = retag (typeName @a)
  baseOid = retag (baseOid @a)
  arrayOid = retag (arrayOid @a)
  typeParams = retag (typeParams @a)
  binaryEncoder = binaryEncoder @a . to
  binaryDecoder =
    binaryDecoder @a <&> \case
      Left err -> Left err
      Right value ->
        tryFrom
          ( DecodingError
              { location = ["ViaIsSome"],
                reason =
                  UnsupportedValueDecodingErrorReason
                    ""
                    (to @Text (textualEncoder @a value))
              }
          )
          value
  textualEncoder = textualEncoder @a . to
  textualDecoder = do
    value <- textualDecoder @a
    case maybeFrom value of
      Just b -> pure (ViaIsSome b)
      Nothing -> fail "ViaIsSome: value out of range"

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
