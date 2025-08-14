module PostgresqlTypes.Types.Jsonb (Jsonb (..)) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson.Text
import qualified Jsonifier
import qualified JsonifierAeson
import qualified PeekyBlinders
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import qualified PostgresqlTypes.Types.Jsonb.QuickCheckGens as QuickCheckGens
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

newtype Jsonb = Jsonb Aeson.Value
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaPostgresqlType Jsonb)

instance Arbitrary Jsonb where
  arbitrary = Jsonb <$> QuickCheckGens.value
  shrink (Jsonb aesonValue) = Jsonb <$> QuickCheck.shrink aesonValue

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

toAesonValue :: Jsonb -> Aeson.Value
toAesonValue (Jsonb value) = value
