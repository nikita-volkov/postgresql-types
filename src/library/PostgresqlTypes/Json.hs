module PostgresqlTypes.Json
  ( Json,

    -- * Accessors
    toValue,

    -- * Constructors
    fromValue,
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
import qualified TextBuilder

-- | PostgreSQL @json@ type.
--
-- Stores JSON data as text, unlike @jsonb@ which stores
-- it in a binary format. This means @json@ preserves the exact textual
-- representation including whitespace and key ordering.
-- However it is less efficient for both storage and processing.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-json.html).
newtype Json = Json Aeson.Value
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaIsScalar Json)

instance Arbitrary Json where
  arbitrary = fromAesonValue <$> arbitrary
  shrink = fmap Json . shrink . toAesonValue

instance IsScalar Json where
  typeName = Tagged "json"
  baseOid = Tagged (Just 114)
  arrayOid = Tagged (Just 199)
  typeParams = Tagged []
  binaryEncoder =
    -- JSON type stores as UTF-8 text without version byte prefix
    Jsonifier.toWrite . JsonifierAeson.aesonValue . toAesonValue
  binaryDecoder = do
    jsonBytes <- PtrPeeker.remainderAsByteString
    pure
      ( bimap
          ( \string ->
              DecodingError
                { location = ["json"],
                  reason =
                    ParsingDecodingErrorReason
                      (fromString string)
                      jsonBytes
                }
          )
          Json
          (Aeson.eitherDecodeStrict jsonBytes)
      )
  textualEncoder =
    TextBuilder.lazyText . Aeson.Text.encodeToLazyText . toAesonValue
  textualDecoder = do
    jsonText <- Attoparsec.takeText
    case Aeson.eitherDecodeStrict (Text.Encoding.encodeUtf8 jsonText) of
      Left err -> fail err
      Right value -> pure (Json value)

-- * Accessors

-- | Extract the underlying 'Aeson.Value'.
toValue :: Json -> Aeson.Value
toValue (Json value) = value

-- * Constructors

-- | Construct from Aeson Value by filtering out null characters from every string and object key.
fromValue :: Aeson.Value -> Json
fromValue = Json . updateValue
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

-- Legacy names for backward compatibility
toAesonValue :: Json -> Aeson.Value
toAesonValue = toValue

fromAesonValue :: Aeson.Value -> Json
fromAesonValue = fromValue
