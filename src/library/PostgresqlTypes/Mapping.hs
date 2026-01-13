module PostgresqlTypes.Mapping
  ( -- * Mapping classes
    ParameterizesStatement (..),
    MapsToScalar (..),

    -- * Errors
    DecodingError (..),
    DecodingErrorReason (..),

    -- * Parameters
    Params (..),
    param,

    -- * Result sets
    Result (..),
    single,
    multirow,
    rowsAffected,

    -- ** Result set columns
    Columns (..),
    column,

    -- * Nullability
    Nullability,
    nonNullable,
    nullable,

    -- * Dimensionality
    Dimensionality,
    d0,
    d1,
    d2,

    -- ** Dimension
    Dimension,
    vector,

    -- * Scalar
    Scalar,
    enum,
    composite,

    -- ** Composite fields
    Fields,
    field,

    -- * Primitive types
    module PostgresqlTypes.Mapping.Primitives,
  )
where

import PostgresqlTypes.Codec hiding (column, field, param)
import PostgresqlTypes.Mapping.MapsToScalar
import PostgresqlTypes.Mapping.ParameterizesStatement
import PostgresqlTypes.Mapping.Primitives
