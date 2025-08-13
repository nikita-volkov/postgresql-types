-- | Extension of \"jsonifier\" integrating it with \"aeson\".
--
-- Can be imported in the same namespace as "Jsonifier" itself.
module JsonifierAeson where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import Data.Bifunctor (bimap)
import Jsonifier
import Prelude hiding (null)

aesonValue :: Aeson.Value -> Json
aesonValue = \case
  Aeson.Null -> null
  Aeson.Bool b -> bool b
  Aeson.Number n -> scientificNumber n
  Aeson.String s -> textString s
  Aeson.Array a -> aesonArray a
  Aeson.Object o -> aesonObject o

aesonArray :: Aeson.Array -> Json
aesonArray = array . fmap aesonValue

aesonObject :: Aeson.Object -> Json
aesonObject = object . fmap (bimap Aeson.Key.toText aesonValue) . Aeson.KeyMap.toList
