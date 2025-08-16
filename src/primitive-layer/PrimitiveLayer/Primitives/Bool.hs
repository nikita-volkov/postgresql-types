module PrimitiveLayer.Primitives.Bool (Bool (..)) where

import qualified Data.Bool
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude hiding (Bool)
import qualified PtrPoker.Write as Write
import qualified TextBuilder

newtype Bool = Bool Data.Bool.Bool
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaPrimitive Bool)

instance Primitive Bool where
  typeName = Tagged "bool"
  baseOid = Tagged 16
  arrayOid = Tagged 1000
  binaryEncoder (Bool b) = Write.word8 (if b then 1 else 0)
  binaryDecoder =
    PeekyBlinders.statically do
      b <- PeekyBlinders.unsignedInt1
      pure (Right (Bool (b /= 0)))
  textualEncoder (Bool b) = if b then "t" else "f"
