module PrimitiveLayer.Primitives.Jsonb (Jsonb) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.Aeson.Text as Aeson.Text
import qualified Data.Text as Text
import qualified Jsonifier
import qualified JsonifierAeson
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import qualified TextBuilder

newtype Jsonb = Jsonb Aeson.Value
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaPostgresqlType Jsonb)

instance Arbitrary Jsonb where
  arbitrary = fromAesonValue <$> arbitrary
  shrink = fmap Jsonb . shrink . toAesonValue

instance PostgresqlType Jsonb where
  mapping =
    Mapping
      { schemaName = Nothing,
        typeName = "jsonb",
        baseOid = Just 3802,
        arrayOid = Just 3807,
        binaryEncoder =
          mappend (Write.word8 1) . Jsonifier.toWrite . JsonifierAeson.aesonValue . toAesonValue,
        binaryDecoder = do
          firstByte <- PeekyBlinders.statically PeekyBlinders.unsignedInt1
          case firstByte of
            1 -> do
              remainingBytes <- PeekyBlinders.remainderAsByteString
              pure
                ( bimap
                    ( \string ->
                        DecodingError
                          { location = ["json"],
                            reason =
                              ParsingDecodingErrorReason
                                (fromString string)
                                remainingBytes
                          }
                    )
                    Jsonb
                    (Aeson.eitherDecodeStrict remainingBytes)
                )
            _ ->
              pure
                ( Left
                    ( DecodingError
                        { location = ["json-encoding-format"],
                          reason =
                            UnexpectedValueDecodingErrorReason
                              "1"
                              (TextBuilder.toText (TextBuilder.decimal firstByte))
                        }
                    )
                ),
        textualEncoder =
          TextBuilder.lazyText . Aeson.Text.encodeToLazyText . toAesonValue
      }

instance IsSome Aeson.Value Jsonb where
  to = toAesonValue
  maybeFrom = maybeFromAesonValue

instance IsMany Aeson.Value Jsonb where
  from = fromAesonValue

instance IsSome Jsonb Aeson.Value where
  to = fromAesonValue
  maybeFrom = Just . toAesonValue

instance IsMany Jsonb Aeson.Value where
  from = toAesonValue

toAesonValue :: Jsonb -> Aeson.Value
toAesonValue (Jsonb value) = value

-- | Construct from Aeson Value by filtering out null characters from every string and object key.
fromAesonValue :: Aeson.Value -> Jsonb
fromAesonValue = Jsonb . updateValue
  where
    updateValue = \case
      Aeson.String string -> Aeson.String (updateText string)
      Aeson.Object object -> Aeson.Object (updateObject object)
      Aeson.Array array -> Aeson.Array (updateArray array)
      other -> other
    updateText = Text.replace "\NUL" ""
    updateObject = Aeson.KeyMap.mapKeyVal updateKey updateValue
    updateArray = fmap updateValue
    updateKey = Aeson.Key.fromText . updateText . Aeson.Key.toText

-- | Construct from Aeson Value while failing if any of its strings or object keys contain null characters.
maybeFromAesonValue :: Aeson.Value -> Maybe Jsonb
maybeFromAesonValue = fmap Jsonb . validateValue
  where
    validateValue = \case
      Aeson.String string -> Aeson.String <$> validateText string
      Aeson.Object object -> Aeson.Object <$> validateObject object
      Aeson.Array array -> Aeson.Array <$> validateArray array
      other -> pure other
    validateText text =
      if Text.elem '\NUL' text
        then Nothing
        else Just text
    validateObject = Aeson.KeyMap.traverseWithKey (\key value -> validateKey key *> validateValue value)
    validateArray = traverse validateValue
    validateKey = fmap Aeson.Key.fromText . validateText . Aeson.Key.toText
