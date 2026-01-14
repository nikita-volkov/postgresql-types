module Demo.Types.Genre where

import PostgresqlTypes.Mapping
import Prelude hiding (Int8, Text)

data Genre = Rock | Pop | Jazz
  deriving (Eq, Ord)

instance MapsToScalar Genre where
  scalarOf =
    enum
      ""
      "genre"
      [ ("rock", Rock),
        ("pop", Pop),
        ("jazz", Jazz)
      ]
