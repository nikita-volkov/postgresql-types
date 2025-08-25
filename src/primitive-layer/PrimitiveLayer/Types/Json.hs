module PrimitiveLayer.Types.Json (Json) where

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
import PrimitiveLayer.Via
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @json@ type.
--
-- Stores JSON data as text, unlike @jsonb@ which stores
-- it in a binary format. This means @json@ preserves the exact textual
-- representation including whitespace and key ordering.
-- However it is less efficient for both storage and processing.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-json.html).
newtype Json = Json Aeson.Value
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaPrimitive Json)

instance Arbitrary Json where
  arbitrary = fromAesonValue <$> arbitrary
  shrink = fmap Json . shrink . toAesonValue

instance Mapping Json where
  typeName = Tagged "json"
  baseOid = Tagged 114
  arrayOid = Tagged 199
  binaryEncoder =
    -- JSON type stores as UTF-8 text without version byte prefix
    Jsonifier.toWrite . JsonifierAeson.aesonValue . toAesonValue
  binaryDecoder = do
    jsonBytes <- PeekyBlinders.remainderAsByteString
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

instance IsSome Aeson.Value Json where
  to = toAesonValue
  maybeFrom value = 
    let normalized = fromAesonValue value
    in if toAesonValue normalized == value then Just normalized else Nothing

instance IsMany Aeson.Value Json where
  onfrom = fromAesonValue

toAesonValue :: Json -> Aeson.Value
toAesonValue (Json value) = value

-- | Construct from Aeson Value by filtering out null characters from every string and object key.
fromAesonValue :: Aeson.Value -> Json
fromAesonValue = Json . updateValue
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

-- | Construct from Aeson Value by filtering out null characters from every string and object key.
-- Always succeeds by transforming the input as needed.
maybeFromAesonValue :: Aeson.Value -> Maybe Json
maybeFromAesonValue = Just . fromAesonValue
