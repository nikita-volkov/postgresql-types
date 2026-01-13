module PostgresqlTypes.Mapping.MapsToScalar where

import qualified PostgresqlTypes.Codec as Codec
import qualified PostgresqlTypes.Primitive as Primitive

class MapsToScalar a where
  scalarOf :: Codec.Scalar a

instance MapsToScalar Primitive.Int8 where
  scalarOf = Codec.primitive

instance MapsToScalar Primitive.Text where
  scalarOf = Codec.primitive

instance MapsToScalar Primitive.Timestamptz where
  scalarOf = Codec.primitive

field :: forall a b c. (MapsToScalar a) => Codec.Dimensionality a b -> Codec.Nullability b c -> Codec.Fields c c
field =
  Codec.field (scalarOf @a)

param :: forall a b c. (MapsToScalar a) => Codec.Dimensionality a b -> Codec.Nullability b c -> Codec.Params c
param =
  Codec.param (scalarOf @a)

column :: forall a b c. (MapsToScalar a) => Codec.Dimensionality a b -> Codec.Nullability b c -> Codec.Columns c
column =
  Codec.column (scalarOf @a)
