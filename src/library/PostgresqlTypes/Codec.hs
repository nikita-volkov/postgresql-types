module PostgresqlTypes.Codec
  ( -- * Errors
    DecodingError (..),
    DecodingErrorReason (..),

    -- * Parameter
    Params (..),
    param,

    -- * Result sets
    Result (..),
    oneRow,
    someRow,
    manyRows,
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

    -- * Scalar types
    Scalar,
    primitive,
    enum,
    composite,

    -- * Composite types
    Fields,
    field,
  )
where

import PostgresqlTypes.Codec.Columns
import PostgresqlTypes.Codec.DecodingError
import PostgresqlTypes.Codec.Dimension
import PostgresqlTypes.Codec.Dimensionality
import PostgresqlTypes.Codec.Fields
import PostgresqlTypes.Codec.Nullability
import PostgresqlTypes.Codec.Params
import PostgresqlTypes.Codec.Result
import PostgresqlTypes.Codec.Scalar
