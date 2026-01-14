module PostgresqlTypes.Mapping
  ( -- * Statement mapping
    ParameterizesStatement (..),
    StatementOf (..),

    -- * Scalar mapping
    MapsToScalar (..),

    -- * Errors
    DecodingError (..),
    DecodingErrorReason (..),

    -- * Parameters
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
