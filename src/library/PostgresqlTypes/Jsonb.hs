module PostgresqlTypes.Jsonb
  ( Jsonb,

    -- * Accessors
    toAesonValue,

    -- * Constructors
    normalizeFromAesonValue,
    refineFromAesonValue,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.Aeson.Text as Aeson.Text
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Jsonifier
import qualified JsonifierAeson
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @jsonb@ type. Binary JSON data.
--
-- A more efficient representation than @json@, allowing for faster processing and smaller storage size.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-json.html).
newtype Jsonb = Jsonb Aeson.Value
  deriving newtype (Eq, Ord)
  deriving (Show, Read, IsString) via (ViaIsScalar Jsonb)

instance Arbitrary Jsonb where
  arbitrary = normalizeFromAesonValue <$> arbitrary
  shrink = fmap Jsonb . shrink . toAesonValue

instance IsScalar Jsonb where
  typeName = Tagged "jsonb"
  baseOid = Tagged (Just 3802)
  arrayOid = Tagged (Just 3807)
  typeParams = Tagged []
  binaryEncoder =
    mappend (Write.word8 1) . Jsonifier.toWrite . JsonifierAeson.aesonValue . toAesonValue
  binaryDecoder = do
    firstByte <- PtrPeeker.fixed PtrPeeker.unsignedInt1
    case firstByte of
      1 -> do
        remainingBytes <- PtrPeeker.remainderAsByteString
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
          )
  textualEncoder =
    TextBuilder.lazyText . Aeson.Text.encodeToLazyText . toAesonValue
  textualDecoder = do
    jsonText <- Attoparsec.takeText
    case Aeson.eitherDecodeStrict (Text.Encoding.encodeUtf8 jsonText) of
      Left err -> fail err
      Right value -> pure (Jsonb value)

-- * Accessors

-- | Extract the underlying 'Aeson.Value'.
toAesonValue :: Jsonb -> Aeson.Value
toAesonValue (Jsonb value) = value

-- * Constructors

-- | Construct from Aeson Value while failing if any of its strings or object keys contain null characters.
refineFromAesonValue :: Aeson.Value -> Maybe Jsonb
refineFromAesonValue = fmap Jsonb . validateValue
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

-- | Construct from Aeson Value by filtering out null characters from every string and object key.
normalizeFromAesonValue :: Aeson.Value -> Jsonb
normalizeFromAesonValue = Jsonb . updateValue
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
