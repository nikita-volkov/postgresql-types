module PrimitiveLayer.Primitives.Jsonb (Jsonb) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.Aeson.Text as Aeson.Text
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Jsonifier
import qualified JsonifierAeson
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Vias
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

newtype Jsonb = Jsonb Aeson.Value
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaPrimitive Jsonb)

instance Arbitrary Jsonb where
  arbitrary = do
    -- Generate Aeson.Value and use fromAesonValue to ensure consistency with IsMany
    value <- arbitraryValueWithoutNulls 10  -- Start with a small size to avoid deep nesting
    pure $ from value
    where
      -- Generate Aeson.Value without null characters to avoid discarding too many test cases
      arbitraryValueWithoutNulls maxSize = QuickCheck.sized $ \size ->
        let actualSize = min size maxSize
        in QuickCheck.oneof $
          [ pure Aeson.Null,
            Aeson.Bool <$> arbitrary,
            Aeson.Number <$> arbitrary,
            Aeson.String <$> (Text.pack <$> QuickCheck.listOf1 (QuickCheck.suchThat (QuickCheck.choose (' ', '~')) (/= '\NUL')))
          ] ++
          (if actualSize > 0 then
            [ Aeson.Array <$> (Vector.fromList <$> QuickCheck.resize (actualSize `div` 2) (QuickCheck.listOf (arbitraryValueWithoutNulls (actualSize - 1)))),
              Aeson.Object <$> QuickCheck.resize (actualSize `div` 2) 
                (Aeson.KeyMap.fromList <$> QuickCheck.listOf 
                  ((,) <$> (Aeson.Key.fromText . Text.pack <$> QuickCheck.listOf1 (QuickCheck.suchThat (QuickCheck.choose ('a', 'z')) (/= '\NUL')))
                       <*> arbitraryValueWithoutNulls (actualSize - 1)))
            ]
          else [])
  shrink jsonb = 
    let value = toAesonValue jsonb
        shrunkValues = shrink value
    in map from shrunkValues

instance Primitive Jsonb where
  typeName = Tagged "jsonb"
  baseOid = Tagged 3802
  arrayOid = Tagged 3807
  binaryEncoder =
    mappend (Write.word8 1) . Jsonifier.toWrite . JsonifierAeson.aesonValue . toAesonValue
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
          )
  textualEncoder =
    TextBuilder.lazyText . Aeson.Text.encodeToLazyText . toAesonValue

instance IsSome Aeson.Value Jsonb where
  to = toAesonValue
  maybeFrom = maybeFromAesonValue

instance IsMany Aeson.Value Jsonb where
  from = fromAesonValue

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
