module PqProcedures.Algebra where

import qualified Database.PostgreSQL.LibPQ as Pq
import Prelude

class Procedure params result | params -> result where
  run :: Pq.Connection -> params -> IO result
