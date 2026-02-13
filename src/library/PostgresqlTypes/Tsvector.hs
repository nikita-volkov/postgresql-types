module PostgresqlTypes.Tsvector
  ( Tsvector,

    -- * Accessors
    toLexemeList,

    -- * Constructors
    fromLexemeList,
    normalizeFromLexemeList,

    -- * Weight
    Weight (..),
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Vector as Vector
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | Weight of a tsvector lexeme position.
data Weight = WeightA | WeightB | WeightC | WeightD
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

instance Arbitrary Weight where
  arbitrary = QuickCheck.elements [WeightA, WeightB, WeightC, WeightD]

instance Hashable Weight where
  hashWithSalt salt = hashWithSalt salt . fromEnum

-- | PostgreSQL @tsvector@ type. Full-text search document representation.
--
-- A tsvector is a sorted list of distinct lexemes with optional position and weight information.
-- Lexemes are sorted alphabetically and deduplicated, matching PostgreSQL's canonical representation.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-textsearch.html).
data Tsvector = Tsvector (Vector (Text, Vector (Word16, Weight)))
  deriving stock (Eq, Ord)
  deriving (Show, Read, IsString) via (ViaIsScalar Tsvector)

instance Hashable Tsvector where
  hashWithSalt salt (Tsvector lexemes) =
    Vector.foldl' (\s (t, ps) -> Vector.foldl' (\s' (p, w) -> s' `hashWithSalt` p `hashWithSalt` fromEnum w) (s `hashWithSalt` t) ps) salt lexemes

instance Arbitrary Tsvector where
  arbitrary = do
    size <- QuickCheck.getSize
    numLexemes <- QuickCheck.choose (0, max 0 size)
    lexemeMap <-
      Map.fromList <$> QuickCheck.vectorOf numLexemes do
        -- Generate a non-empty lexeme token without NUL characters
        token <-
          Text.pack
            <$> QuickCheck.listOf1
              (QuickCheck.suchThat arbitrary (\c -> c /= '\NUL'))
        numPositions <- QuickCheck.choose (0, 3)
        positions <-
          sortAndDedupPositions <$> QuickCheck.vectorOf numPositions do
            pos <- QuickCheck.choose (1, 16383)
            weight <- arbitrary
            pure (pos, weight)
        pure (token, positions)
    -- Sort by lexeme (Map.toAscList) and deduplicate (Map guarantees unique keys)
    let sorted = Map.toAscList lexemeMap
    pure (Tsvector (Vector.fromList (map (\(t, ps) -> (t, Vector.fromList ps)) sorted)))
  shrink (Tsvector lexemes) =
    map (\ls -> normalizeLexemes (map (\(t, ps) -> (t, Vector.fromList ps)) ls)) $
      QuickCheck.shrinkList
        ( \(tok, positions) -> do
            shrunkenTok <- QuickCheck.shrinkMap Text.pack Text.unpack tok
            shrunkenPositions <- QuickCheck.shrinkList shrink positions
            pure (shrunkenTok, shrunkenPositions)
        )
        (map (\(t, ps) -> (t, Vector.toList ps)) (Vector.toList lexemes))

-- | Sort lexemes alphabetically and deduplicate by lexeme text, merging positions.
-- Positions within each lexeme are sorted by position number and deduplicated
-- (keeping the highest weight for duplicate positions), matching PostgreSQL's canonical form.
normalizeLexemes :: [(Text, Vector (Word16, Weight))] -> Tsvector
normalizeLexemes lexemes =
  let m = Map.fromListWith (<>) (map (\(t, ps) -> (t, Vector.toList ps)) lexemes)
      sorted = Map.toAscList m
   in Tsvector (Vector.fromList (map (\(t, ps) -> (t, Vector.fromList (sortAndDedupPositions ps))) sorted))

-- | Sort positions by position number ascending, deduplicating by position
-- (keeping the minimum weight, i.e. highest priority: A < B < C < D).
sortAndDedupPositions :: [(Word16, Weight)] -> [(Word16, Weight)]
sortAndDedupPositions =
  map (foldr1 (\(p, w1) (_, w2) -> (p, min w1 w2)))
    . List.groupBy (\a b -> fst a == fst b)
    . List.sortOn fst

instance IsScalar Tsvector where
  schemaName = Tagged Nothing
  typeName = Tagged "tsvector"
  baseOid = Tagged (Just 3614)
  arrayOid = Tagged (Just 3643)
  typeParams = Tagged []

  -- Binary format:
  -- 4 bytes: number of lexemes (int32)
  -- Per lexeme:
  --   N bytes: lexeme text as null-terminated UTF-8 string
  --   2 bytes: number of positions (uint16)
  --   Per position:
  --     2 bytes: uint16 where bits 14-15 = weight (A=3,B=2,C=1,D=0), bits 0-13 = position
  binaryEncoder (Tsvector lexemes) =
    Write.bInt32 (fromIntegral (Vector.length lexemes))
      <> Vector.foldMap encodeLexeme lexemes
    where
      encodeLexeme (token, positions) =
        let tokenBytes = Text.Encoding.encodeUtf8 token
         in Write.byteString tokenBytes
              <> Write.word8 0 -- null terminator
              <> Write.bWord16 (fromIntegral (Vector.length positions))
              <> Vector.foldMap encodePosition positions
      encodePosition (pos, weight) =
        let weightBits = case weight of
              WeightA -> 3
              WeightB -> 2
              WeightC -> 1
              WeightD -> 0
            -- PostgreSQL tsvector positions must be in the range 1..16383.
            -- Clamp here to avoid silent truncation by bit masking.
            posClamped = max 1 (min 16383 pos)
         in Write.bWord16 ((weightBits `shiftL` 14) .|. posClamped)

  binaryDecoder = runExceptT do
    numLexemes <- lift $ PtrPeeker.fixed PtrPeeker.beSignedInt4
    if numLexemes < 0
      then
        throwError
          ( DecodingError
              { location = ["tsvector", "lexemeCount"],
                reason =
                  ParsingDecodingErrorReason
                    (fromString "Negative lexeme count in tsvector binary data")
                    ByteString.empty
              }
          )
      else do
        lexemes <- Vector.fromList <$> replicateM (fromIntegral numLexemes) decodeLexeme
        pure (Tsvector lexemes)
    where
      decodeLexeme = do
        -- Read null-terminated UTF-8 string
        tokenBytes <- lift PtrPeeker.nullTerminatedStringAsByteString
        case Text.Encoding.decodeUtf8' tokenBytes of
          Left e ->
            throwError
              ( DecodingError
                  { location = ["tsvector", "lexeme"],
                    reason = ParsingDecodingErrorReason (fromString (show e)) tokenBytes
                  }
              )
          Right token
            | Text.null token ->
                throwError
                  ( DecodingError
                      { location = ["tsvector", "lexeme"],
                        reason =
                          ParsingDecodingErrorReason
                            (fromString "empty lexeme is not allowed in tsvector")
                            tokenBytes
                      }
                  )
            | otherwise -> do
                numPositions <- lift $ PtrPeeker.fixed PtrPeeker.beUnsignedInt2
                positions <-
                  Vector.fromList <$> replicateM (fromIntegral numPositions) do
                    posWord <- lift $ PtrPeeker.fixed PtrPeeker.beUnsignedInt2
                    let weightBits = (posWord `shiftR` 14) .&. 0x3
                    let weight = case weightBits of
                          3 -> WeightA
                          2 -> WeightB
                          1 -> WeightC
                          _ -> WeightD
                    let pos = posWord .&. 0x3FFF
                    pure (pos, weight)
                pure (token, positions)

  -- Text format: 'lexeme1':1A,2B 'lexeme2':3C
  -- Single quotes are escaped as '', backslashes as \\
  textualEncoder (Tsvector lexemes) =
    TextBuilder.intercalateMap " " encodeLexeme (Vector.toList lexemes)
    where
      encodeLexeme (token, positions) =
        TextBuilder.char '\''
          <> TextBuilder.text (escapeToken token)
          <> TextBuilder.char '\''
          <> if Vector.null positions
            then mempty
            else TextBuilder.char ':' <> TextBuilder.intercalateMap "," encodePosition (Vector.toList positions)
      encodePosition (pos, weight) =
        TextBuilder.string (show pos)
          <> case weight of
            WeightA -> TextBuilder.char 'A'
            WeightB -> TextBuilder.char 'B'
            WeightC -> TextBuilder.char 'C'
            WeightD -> mempty -- D is default, omitted by PostgreSQL
      escapeToken = Text.concatMap escapeChar
      escapeChar c = case c of
        '\'' -> "''"
        '\\' -> "\\\\"
        _ -> Text.singleton c

  textualDecoder = do
    -- Allow and ignore leading whitespace before the first lexeme
    Attoparsec.skipSpace
    lexemes <- lexemeParser `Attoparsec.sepBy` space1
    -- Allow and ignore trailing whitespace after the last lexeme
    Attoparsec.skipSpace
    -- Sort and deduplicate to match PostgreSQL's canonical form
    let Tsvector normalized = normalizeLexemes (map (\(t, ps) -> (t, Vector.fromList ps)) lexemes)
    pure (Tsvector normalized)
    where
      -- Consume one or more space / tab / newline characters between lexemes
      space1 = do
        _ <- Attoparsec.takeWhile1 (\c -> c == ' ' || c == '\t' || c == '\n')
        pure ()
      lexemeParser = do
        _ <- Attoparsec.char '\''
        token <- parseToken
        _ <- Attoparsec.char '\''
        positions <- parsePositions <|> pure []
        pure (token, positions)
      parseToken = do
        chars <- many (escapedQuote <|> escapedBackslash <|> normalChar)
        pure (Text.pack chars)
      escapedQuote = do
        _ <- Attoparsec.string "''"
        pure '\''
      escapedBackslash = do
        _ <- Attoparsec.string "\\\\"
        pure '\\'
      normalChar = Attoparsec.satisfy (\c -> c /= '\'' && c /= '\\')
      parsePositions = do
        _ <- Attoparsec.char ':'
        parsePosition `Attoparsec.sepBy1` Attoparsec.char ','
      parsePosition = do
        pos <- Attoparsec.decimal
        weight <-
          (Attoparsec.char 'A' >> pure WeightA)
            <|> (Attoparsec.char 'B' >> pure WeightB)
            <|> (Attoparsec.char 'C' >> pure WeightC)
            <|> (Attoparsec.char 'D' >> pure WeightD)
            <|> pure WeightD -- default weight
        pure (pos, weight)

-- * Accessors

-- | Extract the tsvector as a list of (lexeme, positions) pairs.
-- Lexemes are in sorted order. Each position is a (position, weight) pair where position is 1-16383.
toLexemeList :: Tsvector -> [(Text, [(Word16, Weight)])]
toLexemeList (Tsvector lexemes) =
  map (\(t, ps) -> (t, Vector.toList ps)) (Vector.toList lexemes)

-- * Constructors

-- | Construct a tsvector from a list of (lexeme, positions) pairs with validation.
-- Returns 'Nothing' if any lexeme is empty or contains null characters.
-- Sorts and deduplicates lexemes to match PostgreSQL's canonical representation.
fromLexemeList :: [(Text, [(Word16, Weight)])] -> Maybe Tsvector
fromLexemeList lexemes =
  if any (\(t, _) -> Text.null t || Text.elem '\NUL' t) lexemes
    then Nothing
    else Just (normalizeLexemes (map (\(t, ps) -> (t, Vector.fromList ps)) lexemes))

-- | Construct a tsvector from a list of (lexeme, positions) pairs.
-- Strips null characters from lexemes and removes empty lexemes.
-- Sorts and deduplicates lexemes to match PostgreSQL's canonical representation.
normalizeFromLexemeList :: [(Text, [(Word16, Weight)])] -> Tsvector
normalizeFromLexemeList lexemes =
  let cleaned = filter (\(t, _) -> not (Text.null t)) $ map (\(t, ps) -> (Text.replace "\NUL" "" t, ps)) lexemes
   in normalizeLexemes (map (\(t, ps) -> (t, Vector.fromList ps)) cleaned)
