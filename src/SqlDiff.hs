{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module SqlDiff
    where

import Data.Aeson
import qualified Data.Aeson.TH as TH
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Char (toLower, toUpper, isSpace)
import Data.List (nub, find, intercalate, isInfixOf, isPrefixOf, stripPrefix)
import Data.Maybe (mapMaybe, catMaybes, fromMaybe, isJust, listToMaybe)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Generics (Generic)
import System.FilePath (takeExtension)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)

recordOptions :: Maybe T.Text ->  Options
recordOptions Nothing = defaultOptions {fieldLabelModifier = camelTo2 '_', omitNothingFields = True}
recordOptions (Just prefix) =
  defaultOptions
    { fieldLabelModifier = camelTo2 '_' . removePrefix
    , omitNothingFields = True
    }
    where
        removePrefix field = fromMaybe field $ stripPrefix (T.unpack prefix) field

data ColumnDef = ColumnDef
    { colName :: String
    , colType :: String
    , colNullable :: Bool
    , colDefault :: Maybe String
    } deriving (Show, Eq, Generic)

instance ToJSON ColumnDef where
    toJSON = genericToJSON (recordOptions (Just "col"))

data TableDef = TableDef
    { tableName :: String
    , columns :: [ColumnDef]
    , primaryKey :: Maybe [String]
    , foreignKeys :: [(String, String, String)]
    } deriving (Show, Eq, Generic)

instance ToJSON TableDef where
    toJSON t = object
        [ "table_name" .= tableName t
        , "columns" .= columns t
        ]

data TableChange = TableChange
    { changeTableName :: String
    , columnsAdded :: [ColumnDef]
    , columnsRemoved :: [ColumnRemoved]
    , columnsModified :: [ColumnModified]
    } deriving (Show, Eq, Generic)

instance ToJSON TableChange where
    toJSON = genericToJSON (recordOptions (Just "change"))

data ColumnRemoved = ColumnRemoved
    { removedColName :: String
    , removedColType :: String
    } deriving (Show, Eq, Generic)

instance ToJSON ColumnRemoved where
    toJSON = genericToJSON (recordOptions (Just "removedCol"))

data ColumnModified = ColumnModified
    { modifiedColName :: String
    , old :: ColumnInfo
    , new :: ColumnInfo
    } deriving (Show, Eq, Generic)

instance ToJSON ColumnModified where
    toJSON = genericToJSON (recordOptions (Just "modifiedCol"))

data ColumnInfo = ColumnInfo
    { infoType :: String
    , infoNullable :: Bool
    , infoDefault :: Maybe String
    } deriving (Show, Eq, Generic)

instance ToJSON ColumnInfo where
    toJSON = genericToJSON (recordOptions (Just "info"))

data TableChanges = TableChanges
    { tablesAdded :: [TableDef]
    , tablesRemoved :: [TableRemoved]
    , tablesModified :: [TableChange]
    } deriving (Show, Eq, Generic)

instance ToJSON TableChanges where
    toJSON = genericToJSON (recordOptions Nothing)

data TableRemoved = TableRemoved
    { removedTableName :: String
    } deriving (Show, Eq, Generic)

instance ToJSON TableRemoved where
    toJSON = genericToJSON (recordOptions (Just "removed"))

data EnumDef = EnumDef
    { enumName :: String
    , enumValues :: [String]
    } deriving (Show, Eq, Generic)

instance ToJSON EnumDef where
    toJSON = genericToJSON (recordOptions (Just "enum"))

data EnumChanges = EnumChanges
    { enumsAdded :: [EnumDef]
    , enumsRemoved :: [EnumRemoved]
    , enumsModified :: [EnumModified]
    } deriving (Show, Eq, Generic)

instance ToJSON EnumChanges where
    toJSON = genericToJSON (recordOptions Nothing)

data EnumRemoved = EnumRemoved
    { removedEnumName :: String
    } deriving (Show, Eq, Generic)

instance ToJSON EnumRemoved where
    toJSON = genericToJSON (recordOptions (Just "removedEnum"))

data EnumModified = EnumModified
    { modifiedEnumName :: String
    , enumsValuesAdded :: [String]
    , enumsValuesRemoved :: [String]
    } deriving (Show, Eq, Generic)

instance ToJSON EnumModified where
    toJSON em = object
        [ "name" .= modifiedEnumName em
        , "enums_added" .= enumsValuesAdded em
        , "enums_removed" .= enumsValuesRemoved em
        ]

-- =================================Megaparsec Parsers===========================================

type Parser = Parsec Void String
type IsPrimaryKey = Bool
type ErrorMessage = String

sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a -- | Lexeme wrapper
lexeme = L.lexeme sc

symbol :: String -> Parser String -- specific string (case-sensitive), then whitespace
symbol = L.symbol sc

keyword :: String -> Parser String -- Case-insensitive keyword (followed by non-identifier-char)
keyword kw = lexeme $ try $ do
    void $ string' kw
    notFollowedBy alphaNumChar
    return kw

identifier :: Parser String -- table/column name
identifier = lexeme $ try $ doubleQuoted <|> backticked <|> unquoted
  where
    backticked = char '`' *> some (noneOf ['`', '\n', '\r']) <* char '`'
    doubleQuoted = char '"' *> some (noneOf ['"', '\n', '\r']) <* char '"'
    unquoted = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

qualifiedIdentifier :: Parser String -- Schema-qualified identifier (e.g., "schema".table or schema.table)
qualifiedIdentifier = lexeme $ try $ do
    schema <- optional $ try $ identifier <* char '.'
    name <- identifier
    return $ case schema of
        Just s -> s ++ "." ++ name
        Nothing -> name

-- | SQL type (e.g., VARCHAR(100), UUID, DECIMAL(10,2), "schema".type)
sqlType :: Parser String
sqlType = lexeme $ try qualified <|> simple
  where
    simple = do
        base <- some letterChar
        params <- optional $ try $ between (char '(') (char ')') (some (noneOf [')']))
        return $ map toUpper $ case params of
            Just p -> base ++ "(" ++ p ++ ")"
            Nothing -> base
    -- Use 'try' so we backtrack if there's no dot after the first identifier
    qualified = try $ do
        schema <- identifier
        void $ char '.'
        typ <- identifier
        return $ schema ++ "." ++ typ

optionalDefault :: Parser (Maybe String)  -- Capture DEFAULT
optionalDefault = optional $ try $ do
    keyword "DEFAULT"
    value <- consumeDefaultValue
    return value
  where
    consumeDefaultValue = do
        optional sc        -- Skip leading spaces
        parts <- many $ choice
            [ -- Parenthesized expression (like (CURRENT_DATE))
              try $ between (char '(') (char ')') consumeDefaultValue >>= \inner -> return ("(" ++ inner ++ ")")
              -- Function call like NOW()
            , try $ do
                name <- some letterChar
                optional sc
                args <- optional $ between (char '(') (char ')') (many (noneOf [')']))
                return $ name ++ maybe "" (\a -> "(" ++ a ++ ")") args
              -- Quoted string
            , try $ between (char '\'') (char '\'') (many (noneOf ['\'']))
              -- Number
            , try $ some (digitChar <|> char '.' <|> char '-')
              -- Keyword like TRUE, FALSE, NULL
            , try $ some letterChar
              -- Skip spaces between parts
            , try $ some spaceChar
            ]
        return $ concat parts

-- | Parse column constraints, returns (isNullable, isPrimaryKey)
columnConstraints :: Parser (Bool, IsPrimaryKey)
columnConstraints = do
    constrs <- many $ choice
        [ try $ keyword "NOT" *> keyword "NULL" *> pure "NOT NULL"
        , try $ keyword "NULL" *> pure "NULL"
        , try $ keyword "PRIMARY" *> keyword "KEY" *> pure "PRIMARY KEY"
        , try $ keyword "UNIQUE" *> pure "UNIQUE"
        , try $ keyword "AUTO_INCREMENT" *> pure "AUTO_INCREMENT"
        ]
    let isPrimaryKey = "PRIMARY KEY" `elem` constrs
        nullable = not ("NOT NULL" `elem` constrs || isPrimaryKey) -- Column is nullable unless NOT NULL or PRIMARY KEY is specified
    return (nullable, isPrimaryKey)

columnDef :: Parser (ColumnDef, IsPrimaryKey)
columnDef = do
    name <- identifier
    colTyp <- sqlType
    (nullable, isPK) <- columnConstraints
    defVal <- optionalDefault
    let col = ColumnDef
            { colName = name
            , colType = colTyp
            , colNullable = nullable
            , colDefault = defVal
            }
    return (col, isPK)

data TableElement = Col ColumnDef IsPrimaryKey | PK [String] | FK (String, String, String) | UQ [String] | Other     -- columns or constraints
    deriving (Show)

-- | Parse a table-level constraint and return constraint info
tableConstraint :: Parser TableElement
tableConstraint = try $ do
    choice
        [ parsePrimaryKey
        , parseForeignKey
        , parseUnique
        , parseOtherConstraint
        ]

parsePrimaryKey :: Parser TableElement
parsePrimaryKey = try $ do
    optional $ try (keyword "CONSTRAINT" *> identifier)
    keyword "PRIMARY"
    keyword "KEY"
    cols <- between (symbol "(") (symbol ")") (sepBy identifier (symbol ","))
    return $ PK cols

parseUnique :: Parser TableElement
parseUnique = try $ do
    optional $ try (keyword "CONSTRAINT" *> identifier)
    keyword "UNIQUE"
    cols <- between (symbol "(") (symbol ")") (sepBy identifier (symbol ","))
    return $ UQ cols

parseForeignKey :: Parser TableElement
parseForeignKey = try $ do
    optional $ try (keyword "CONSTRAINT" *> identifier)
    keyword "FOREIGN"
    keyword "KEY"
    localCols <- between (symbol "(") (symbol ")") (sepBy identifier (symbol ","))
    keyword "REFERENCES"
    refTable <- qualifiedIdentifier
    refCols <- between (symbol "(") (symbol ")") (sepBy identifier (symbol ","))
    case (localCols, refCols) of
        (lc:_, rc:_) -> return $ FK (lc, refTable, rc)
        _ -> fail "Foreign key must specify at least one column"

parseOtherConstraint :: Parser TableElement
parseOtherConstraint = try $ do
    keyword "CONSTRAINT"
    void $ manyTill anySingle (lookAhead (char ',' <|> char ')'))
    return Other

createTable :: Parser TableDef  -- CREATE TABLE statement parser
createTable = between sc (optional sc) $ do
    keyword "CREATE"
    optional $ keyword "TEMPORARY"
    keyword "TABLE"
    optional $ try $ keyword "IF" *> keyword "NOT" *> keyword "EXISTS"
    name <- qualifiedIdentifier
    void $ symbol "("
    -- Parse comma-separated columns/constraints
    -- Try tableConstraint first for keywords like CONSTRAINT, UNIQUE, PRIMARY, FOREIGN, Then fall back to columnDef
    elements <- sepBy (try tableConstraint <|> uncurry Col <$> columnDef) (symbol ",")
    optional $ symbol ","
    void $ symbol ")"
    -- Skip trailing stuff (ENGINE, CHARSET, etc.) until semicolon or EOF
    void $ manyTill anySingle (void (char ';') <|> void (lookAhead (keyword "CREATE")) <|> eof)
    -- Extract columns and constraints from elements
    let columns = [c | Col c _ <- elements]
        pkCols = [cols | PK cols <- elements]
        fkCols = [fk | FK fk <- elements]
        -- Collect column-level PRIMARY KEY constraints
        colLevelPKs = [colName c | Col c isPK <- elements, isPK]
        -- Combine table-level and column-level PKs
        allPKs = if null colLevelPKs then pkCols else [colLevelPKs]
    return TableDef
        { tableName = name
        , columns = columns
        , primaryKey = listToMaybe allPKs
        , foreignKeys = fkCols
        }

createEnum :: Parser EnumDef  -- CREATE TYPE ... AS ENUM parser
createEnum = between sc (optional sc) $ do
    keyword "CREATE"
    keyword "TYPE"
    name <- qualifiedIdentifier
    keyword "AS"
    keyword "ENUM"
    values <- between (symbol "(") (symbol ")") (sepBy enumValue (symbol ","))
    return EnumDef
        { enumName = name
        , enumValues = values
        }

enumValue :: Parser String -- single enum value (quoted string)
enumValue = lexeme $ between (char '\'') (char '\'') (many (noneOf ['\'']))

data SQLElement = TableElement TableDef | EnumElement EnumDef

sqlFile :: Parser ([TableDef], [EnumDef], [ErrorMessage])  -- (tables, enums, error messages)
sqlFile = do
    sc
    stmts <- splitStatements
    results <- mapM parseStatement stmts
    eof
    let tables = [t | Just (Right (TableElement t)) <- results]
        enums = [e | Just (Right (EnumElement e)) <- results]
        errors = [err | Just (Left err) <- results]
    return (tables, enums, errors)

-- | Split SQL into rough statements (handling semicolons inside parens)
splitStatements :: Parser [String]
splitStatements = do
    optional sc
    end <- isJust <$> optional eof
    if end
        then return []
        else do
            stmt <- collectStatement
            rest <- splitStatements
            return (stmt : rest)

collectStatement :: Parser String
collectStatement = reverse <$> go 0 []
  where
    go :: Int -> String -> Parser String
    go depth acc = do
        end <- isJust <$> optional eof
        if end
            then return acc
            else do
                c <- anySingle
                case c of
                    '(' -> go (depth + 1) (c : acc)
                    ')' -> go (max 0 (depth - 1)) (c : acc)
                    ';' | depth == 0 -> return acc
                    _   -> go depth (c : acc)

-- | Parse a single statement - either CREATE TABLE, CREATE TYPE, or ignore
-- Returns Nothing for non-target statements
-- Returns Just (Left error) for statements that failed to parse
-- Returns Just (Right element) for successfully parsed statements
parseStatement :: String -> Parser (Maybe (Either String SQLElement))
parseStatement stmt = do
    let trimmed = trim stmt
        upperTrimmed = map toUpper trimmed
        isCreateTable = "CREATE TABLE" `isPrefixOf` upperTrimmed
        isCreateType = "CREATE TYPE" `isPrefixOf` upperTrimmed
    if isCreateTable
        then case parse createTable "" trimmed of
            Left err -> return $ Just $ Left $ "CREATE TABLE parse failed:\n" ++ errorBundlePretty err ++ "\nStatement was:\n" ++ take 200 trimmed
            Right t -> return $ Just $ Right $ TableElement t
        else if isCreateType
            then case parse createEnum "" trimmed of
                Left err -> return $ Just $ Left $ "CREATE TYPE parse failed:\n" ++ errorBundlePretty err ++ "\nStatement was:\n" ++ take 200 trimmed
                Right e -> return $ Just $ Right $ EnumElement e
            else return Nothing

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

eitherParseSqlFile :: String -> Either ErrorMessage ([TableDef], [EnumDef], [ErrorMessage])
eitherParseSqlFile content =
    case parse sqlFile "" content of
        Left err -> Left $ errorBundlePretty err
        Right (tables, enums, errors) -> Right (tables, enums, errors)

parseSqlFile :: String -> ([TableDef], [EnumDef], [ErrorMessage])
parseSqlFile content =
    case parse sqlFile "" content of
        Left _ -> ([], [], [])
        Right (tables, enums, errors) -> (tables, enums, errors)

extractTables :: [String] -> [TableDef]
extractTables contents = concatMap ((\(tables, _, _) -> tables) . parseSqlFile) contents

extractEnums :: [String] -> [EnumDef]
extractEnums contents = concatMap ((\(_, enums, _) -> enums) . parseSqlFile) contents

compareSqlSchemas :: [TableDef] -> [TableDef] -> TableChanges
compareSqlSchemas oldTables newTables =
    let oldMap = [(tableName t, t) | t <- oldTables]
        newMap = [(tableName t, t) | t <- newTables]

        added = [t | (name, t) <- newMap, name `notElem` map fst oldMap]  -- in new but not in old
        removed = [TableRemoved name | (name, _) <- oldMap, name `notElem` map fst newMap]  -- in old but not in new
        modified = mapMaybe (compareTable oldMap) newMap  -- potentially modified (in both)
    in TableChanges
        { tablesAdded = added
        , tablesRemoved = removed
        , tablesModified = modified
        }
  where
    compareTable :: [(String, TableDef)] -> (String, TableDef) -> Maybe TableChange
    compareTable oldMap (name, newTable) =
        case lookup name oldMap of
            Nothing -> Nothing  -- New table, not modified
            Just oldTable ->
                let (added, removed, modifiedCols) = compareColumns (columns oldTable) (columns newTable)
                in if null added && null removed && null modifiedCols
                   then Nothing  -- No changes
                   else Just TableChange
                       { changeTableName = name
                       , columnsAdded = added
                       , columnsRemoved = removed
                       , columnsModified = modifiedCols
                       }

compareColumns :: [ColumnDef] -> [ColumnDef] -> ([ColumnDef], [ColumnRemoved], [ColumnModified])
compareColumns oldCols newCols =
    let oldMap = [(colName c, c) | c <- oldCols]
        newMap = [(colName c, c) | c <- newCols]

        added = [c | (name, c) <- newMap, name `notElem` map fst oldMap]  -- Columns added
        removed = [ColumnRemoved name (colType c) | (name, c) <- oldMap, name `notElem` map fst newMap]  -- Columns removed
        modified = mapMaybe (compareColumn oldMap) newMap   -- potentially modified (same name, different type/nullable)

    in (added, removed, modified)
  where
    compareColumn :: [(String, ColumnDef)] -> (String, ColumnDef) -> Maybe ColumnModified
    compareColumn oldMap (name, newCol) =
        case lookup name oldMap of
            Nothing -> Nothing
            Just oldCol ->
                if colType oldCol /= colType newCol
                    || colNullable oldCol /= colNullable newCol
                    || colDefault oldCol /= colDefault newCol
                then Just ColumnModified
                    { modifiedColName = name
                    , old = ColumnInfo (colType oldCol) (colNullable oldCol) (colDefault oldCol)
                    , new = ColumnInfo (colType newCol) (colNullable newCol) (colDefault newCol)
                    }
                else Nothing

compareEnums :: [EnumDef] -> [EnumDef] -> EnumChanges
compareEnums oldEnums newEnums =
    let oldMap = [(enumName e, e) | e <- oldEnums]
        newMap = [(enumName e, e) | e <- newEnums]

        added = [e | (name, e) <- newMap, name `notElem` map fst oldMap]  -- (in new but not in old)
        removed = [EnumRemoved name | (name, _) <- oldMap, name `notElem` map fst newMap]  -- (in old but not in new)
        modified = mapMaybe (compareEnum oldMap) newMap  -- potentially modified (in both but values changed)
    in EnumChanges
        { enumsAdded = added
        , enumsRemoved = removed
        , enumsModified = modified
        }
  where
    compareEnum :: [(String, EnumDef)] -> (String, EnumDef) -> Maybe EnumModified
    compareEnum oldMap (name, newEnum) =
        case lookup name oldMap of
            Nothing -> Nothing  -- New enum, not modified
            Just oldEnum ->
                let oldVals = enumValues oldEnum
                    newVals = enumValues newEnum
                    added = [v | v <- newVals, v `notElem` oldVals]
                    removed = [v | v <- oldVals, v `notElem` newVals]
                in if not (null added && null removed)
                then Just EnumModified
                    { modifiedEnumName = name
                    , enumsValuesAdded = added
                    , enumsValuesRemoved = removed
                    }
                else Nothing

processSqlFiles :: [(FilePath, String)] -> [(FilePath, String)] -> IO (TableChanges, EnumChanges)
processSqlFiles oldFiles newFiles = do
    let oldContents = map snd oldFiles
        newContents = map snd newFiles

        oldTables = extractTables oldContents
        newTables = extractTables newContents

        oldEnums = extractEnums oldContents
        newEnums = extractEnums newContents
    return (compareSqlSchemas oldTables newTables, compareEnums oldEnums newEnums)