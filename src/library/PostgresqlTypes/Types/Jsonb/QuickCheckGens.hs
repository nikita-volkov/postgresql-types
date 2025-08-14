module PostgresqlTypes.Types.Jsonb.QuickCheckGens where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.Text as Text
import PostgresqlTypes.Prelude
import Test.QuickCheck

-- | Modification of the standard Arbitrary instance, which avoids null characters to satisfy the limitations of Postgres.
value :: Gen Aeson.Value
value =
  updateValue <$> arbitrary
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
