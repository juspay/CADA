{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields, OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module SqlDiff
    where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Char (toLower, toUpper, isSpace, isAlphaNum, isUpper)
import Data.List (nub, find, intercalate, isPrefixOf, stripPrefix, delete, isInfixOf, takeWhile)
import Data.Maybe (mapMaybe, catMaybes, fromMaybe, isJust, listToMaybe)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)
import Text.Megaparsec.Error (errorBundlePretty)

import qualified Language.SQL.SimpleSQL.Parse as SSP
import qualified Language.SQL.SimpleSQL.Dialect as SSD
import qualified Language.SQL.SimpleSQL.Syntax as SSS

recordOptions :: Maybe T.Text ->  Options
recordOptions Nothing = defaultOptions {fieldLabelModifier = camelTo2 '_', omitNothingFields = True}
recordOptions (Just prefix) =
  defaultOptions
    { fieldLabelModifier = camelTo2 '_' . removePrefix
    , omitNothingFields = True
    }
  where
      removePrefix field = fromMaybe field $ stripPrefix (T.unpack prefix) field

instance ToJSON SSS.Name where
    toJSON = toJSON . show

instance FromJSON SSS.Name where
    parseJSON = withText "Name" $ \t ->
        case reads (T.unpack t) of
            [(x, "")] -> pure x
            _ -> fail $ "Could not parse Name: " ++ T.unpack t

instance ToJSON SSS.TypeName where
    toJSON = toJSON . show

instance FromJSON SSS.TypeName where
    parseJSON = withText "TypeName" $ \t ->
        case reads (T.unpack t) of
            [(x, "")] -> pure x
            _ -> fail $ "Could not parse TypeName: " ++ T.unpack t

instance ToJSON SSS.ScalarExpr where
    toJSON = toJSON . show

instance FromJSON SSS.ScalarExpr where
    parseJSON = withText "ScalarExpr" $ \t ->
        case reads (T.unpack t) of
            [(x, "")] -> pure x
            _ -> fail $ "Could not parse ScalarExpr: " ++ T.unpack t

instance ToJSON SSS.ColConstraint where
    toJSON = toJSON . show

instance FromJSON SSS.ColConstraint where
    parseJSON = withText "ColConstraint" $ \t ->
        case reads (T.unpack t) of
            [(x, "")] -> pure x
            _ -> fail $ "Could not parse ColConstraint: " ++ T.unpack t

instance ToJSON SSS.ColConstraintDef where
    toJSON = toJSON . show

instance FromJSON SSS.ColConstraintDef where
    parseJSON = withText "ColConstraintDef" $ \t ->
        case reads (T.unpack t) of
            [(x, "")] -> pure x
            _ -> fail $ "Could not parse ColConstraintDef: " ++ T.unpack t

instance ToJSON SSS.TableElement where
    toJSON = toJSON . show

instance FromJSON SSS.TableElement where
    parseJSON = withText "TableElement" $ \t ->
        case reads (T.unpack t) of
            [(x, "")] -> pure x
            _ -> fail $ "Could not parse TableElement: " ++ T.unpack t

instance ToJSON SSS.TableConstraint where
    toJSON = toJSON . show

instance FromJSON SSS.TableConstraint where
    parseJSON = withText "TableConstraint" $ \t ->
        case reads (T.unpack t) of
            [(x, "")] -> pure x
            _ -> fail $ "Could not parse TableConstraint: " ++ T.unpack t

instance ToJSON SSS.DefaultClause where
    toJSON = toJSON . show

instance FromJSON SSS.DefaultClause where
    parseJSON = withText "DefaultClause" $ \t ->
        case reads (T.unpack t) of
            [(x, "")] -> pure x
            _ -> fail $ "Could not parse DefaultClause: " ++ T.unpack t

instance ToJSON SSS.ColumnDef where
    toJSON = toJSON . show

instance FromJSON SSS.ColumnDef where
    parseJSON = withText "ColumnDef" $ \t ->
        case reads (T.unpack t) of
            [(x, "")] -> pure x
            _ -> fail $ "Could not parse ColumnDef: " ++ T.unpack t

instance ToJSON SSS.Statement where
    toJSON = toJSON . show

instance FromJSON SSS.Statement where
    parseJSON = withText "Statement" $ \t ->
        case reads (T.unpack t) of
            [(x, "")] -> pure x
            _ -> fail $ "Could not parse Statement: " ++ T.unpack t

-- ========== Diff output types ==========

data AllTableChanges = AllTableChanges
    { tablesAdded :: [SSS.Statement]
    , tablesRemoved :: [TableRemoved]
    , tablesModified :: [TableModified]
    } deriving (Show, Eq, Generic)

instance ToJSON AllTableChanges where
    toJSON = genericToJSON (recordOptions Nothing)

instance FromJSON AllTableChanges where
    parseJSON = genericParseJSON (recordOptions Nothing)

data TableModified = TableModified
    { tableName :: String
    , columnsAdded :: [SSS.ColumnDef]
    , columnsRemoved :: [ColumnRemoved]
    , columnsModified :: [ColumnModified]
    } deriving (Show, Eq, Generic)

instance ToJSON TableModified where
    toJSON = genericToJSON (recordOptions Nothing)

instance FromJSON TableModified where
    parseJSON = genericParseJSON (recordOptions Nothing)

data ColumnRemoved = ColumnRemoved
    { colName :: String
    } deriving (Show, Eq, Generic)

instance ToJSON ColumnRemoved where
    toJSON = genericToJSON (recordOptions (Just "col"))

instance FromJSON ColumnRemoved where
    parseJSON = genericParseJSON (recordOptions (Just "col"))

data ColumnModified = ColumnModified
    { colName :: String
    , old :: SSS.ColumnDef
    , new :: SSS.ColumnDef
    } deriving (Show, Eq, Generic)

instance ToJSON ColumnModified where
    toJSON = genericToJSON (recordOptions (Just "col"))

instance FromJSON ColumnModified where
    parseJSON = genericParseJSON (recordOptions (Just "col"))

data TableRemoved = TableRemoved
    { tableName :: String
    } deriving (Show, Eq, Generic)

instance ToJSON TableRemoved where
    toJSON = genericToJSON (recordOptions Nothing)

instance FromJSON TableRemoved where
    parseJSON = genericParseJSON (recordOptions Nothing)

-- ========== Enum types ==========
data EnumDef = EnumDef
    { enumName :: String
    , enumValues :: [String]
    } deriving (Show, Eq, Generic)

instance ToJSON EnumDef where
    toJSON = genericToJSON (recordOptions (Just "enum"))

instance FromJSON EnumDef where
    parseJSON = genericParseJSON (recordOptions (Just "enum"))

data EnumChanges = EnumChanges
    { enumsAdded :: [EnumDef]
    , enumsRemoved :: [EnumRemoved]
    , enumsModified :: [EnumModified]
    } deriving (Show, Eq, Generic)

instance ToJSON EnumChanges where
    toJSON = genericToJSON (recordOptions Nothing)

instance FromJSON EnumChanges where
    parseJSON = genericParseJSON (recordOptions Nothing)

data EnumRemoved = EnumRemoved
    { removedEnumName :: String
    } deriving (Show, Eq, Generic)

instance ToJSON EnumRemoved where
    toJSON = genericToJSON (recordOptions (Just "removedEnum"))

instance FromJSON EnumRemoved where
    parseJSON = genericParseJSON (recordOptions (Just "removedEnum"))

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

instance FromJSON EnumModified where
    parseJSON = withObject "EnumModified" $ \v -> EnumModified
        <$> v .: "name"
        <*> v .: "enums_added"
        <*> v .: "enums_removed"

-- ========== SSS helper functions ==========
type ErrorMessage = String

typeNameToString :: SSS.TypeName -> String
typeNameToString (SSS.TypeName names) = map toUpper $ namesToString names
typeNameToString (SSS.PrecTypeName names prec) =
    map toUpper (namesToString names) ++ "(" ++ show prec ++ ")"
typeNameToString (SSS.PrecScaleTypeName names prec scale) =
    map toUpper (namesToString names) ++ "(" ++ show prec ++ "," ++ show scale ++ ")"
typeNameToString (SSS.PrecLengthTypeName names prec _ _) =
    map toUpper (namesToString names) ++ "(" ++ show prec ++ ")"
typeNameToString (SSS.CharTypeName names mLen _ _) =
    let base = map toUpper $ namesToString names
    in base ++ maybe "" (\l -> "(" ++ show l ++ ")") mLen
typeNameToString (SSS.TimeTypeName names mPrec _) =
    let base = map toUpper $ namesToString names
    in base ++ maybe "" (\p -> "(" ++ show p ++ ")") mPrec
typeNameToString (SSS.ArrayTypeName tn mLen) =
    typeNameToString tn ++ maybe "[]" (\l -> "[" ++ show l ++ "]") mLen
typeNameToString tn = show tn

-- ========== Statement Splitting ==========
data SplitState = Normal | InString Char

splitStatements :: String -> [String]
splitStatements = filter (not . all isSpace) . go Normal 0 []
  where
    go :: SplitState -> Int -> String -> String -> [String]
    go _ _ acc [] = [reverse acc | not (null acc)]
    go state depth acc (c:cs) = case state of
        Normal
            | "--" `isPrefixOf` (c:cs) ->
                let (_, rest) = span (/= '\n') cs
                in go Normal depth acc rest
            | "/*" `isPrefixOf` (c:cs) ->
                let (_, rest) = takeBlockComment (drop 2 (c:cs))
                in go Normal depth acc rest
            | c == '$' ->
                case takeDollarQuoted (c:cs) of
                    Just (dq, rest) -> go Normal depth (reverse dq ++ acc) rest
                    Nothing -> go Normal depth (c:acc) cs
            | c == '\'' -> go (InString '\'') depth (c:acc) cs
            | c == '"'  -> go (InString '"')  depth (c:acc) cs
            | c == '('  -> go Normal (depth+1) (c:acc) cs
            | c == ')'  -> go Normal (max 0 (depth-1)) (c:acc) cs
            | c == ';' && depth == 0 ->
                reverse acc : go Normal 0 [] cs
            | otherwise -> go Normal depth (c:acc) cs
        InString q
            | c == q -> case cs of
                (q':rest) | q' == q -> go (InString q) depth (q':c:acc) rest
                _ -> go Normal depth (c:acc) cs
            | otherwise -> go (InString q) depth (c:acc) cs

    takeBlockComment s = go' [] s
      where
        go' acc ('*':'/':rest) = ('*':'/':acc, rest)
        go' acc (c:rest) = go' (c:acc) rest
        go' acc [] = (acc, [])

    takeDollarQuoted ('$':cs) =
        let (tag, afterTag) = span isTagChar cs
        in case afterTag of
            ('$':rest) ->
                let endMarker = '$' : tag ++ "$"
                    goBody acc s
                        | endMarker `isPrefixOf` s = (reverse acc, drop (length endMarker) s)
                        | null s = (reverse acc, [])
                        | otherwise = goBody (head s : acc) (tail s)
                    (body, after) = goBody [] rest
                in Just ('$' : tag ++ "$" ++ body ++ endMarker, after)
            _ -> Nothing
      where
        isTagChar c = isAlphaNum c || c == '_'
    takeDollarQuoted _ = Nothing

data SQLElement = TableElement SSS.Statement | EnumElement EnumDef

parseIndividualStatement :: String -> Maybe (Either ErrorMessage SQLElement)
parseIndividualStatement stmt =
    let trimmed = trim stmt
        upperTrimmed = map toUpper trimmed
    in if any (all (flip isInfixOf upperTrimmed)) [["CREATE", " TABLE "], ["CREATE", " TYPE "]]
       then
            case SSP.parseStatement updatedDialect "input" Nothing (T.pack trimmed) of
                Right stmt'@(SSS.CreateTable _ _ _) -> Just $ Right $ TableElement stmt'  -- Successfully Parsed Create Table
                Right _ -> Nothing                                                        -- Successfully Parsed, but not Create Table
                Left err ->                                                               -- Parsing failed, can be enum type
                    case parse createEnum "" trimmed of
                        Right e -> Just $ Right $ EnumElement e                           -- Enum type
                        Left _ -> Just $ Left $ "Parse failed For:\n" ++ trimmed ++ "\nerror : " ++ (T.unpack $ SSP.prettyError err) ++ "\n" -- Not Enum type (Error Message)
       else Nothing
    where
        updatedDialect :: SSD.Dialect
        updatedDialect = SSD.postgres {SSD.diKeywords = delete "language" $ delete "scope" $ SSD.diKeywords SSD.postgres}

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

parseSqlFile :: String -> ([SSS.Statement], [EnumDef], [ErrorMessage])
parseSqlFile content =
    let stmts = splitStatements content
        results = mapMaybe parseIndividualStatement stmts
        tables = [t | Right (TableElement t) <- results]
        enums = [e | Right (EnumElement e) <- results]
        errs = [e | Left e <- results]
    in (tables, enums, errs)

parseSqlFiles :: [String] -> ([SSS.Statement], [EnumDef], [ErrorMessage])
parseSqlFiles files =
    foldl
        (\(tables, enums, errors) file ->
            let (newTables, newEnums, newErrors) = parseSqlFile file
            in (tables ++ newTables, enums ++ newEnums, errors ++ newErrors)
        ) ([], [] , []) files

-- ========== Megaparsec Enum Parser (minimal) ==========
type Parser = Parsec Void String

sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

keyword :: String -> Parser String
keyword kw = lexeme $ try $ do
    void $ string' kw
    notFollowedBy alphaNumChar
    return kw

identifier :: Parser String
identifier = lexeme $ try $ doubleQuoted <|> backticked <|> unquoted
  where
    backticked = char '`' *> some (noneOf ['`', '\n', '\r']) <* char '`'
    doubleQuoted = char '"' *> some (noneOf ['"', '\n', '\r']) <* char '"'
    unquoted = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

qualifiedIdentifier :: Parser String
qualifiedIdentifier = lexeme $ try $ do
    schema <- optional $ try $ identifier <* char '.'
    name <- identifier
    return $ case schema of
        Just s -> s ++ "." ++ name
        Nothing -> name

createEnum :: Parser EnumDef
createEnum = do
    keyword "CREATE"
    keyword "TYPE"
    name <- qualifiedIdentifier
    keyword "AS"
    keyword "ENUM"
    values <- between (symbol "(") (symbol ")") (sepBy enumValue (symbol ","))
    optional $ symbol ";"
    return EnumDef
        { enumName = getTextFromQualifiedText $ map toLower name
        , enumValues = values
        }

getTextFromQualifiedText :: String -> String
getTextFromQualifiedText qualifiedName =
    reverse $ takeWhile (/='.') $ reverse qualifiedName

enumValue :: Parser String
enumValue = lexeme $ between (char '\'') (char '\'') (many (noneOf ['\'']))

-- ========== Utility ==========
parseTableName :: String -> (Maybe String, String)
parseTableName name =
    case break (== '.') name of
        (base, []) -> (Nothing, base)
        (schema, '.':base) -> (Just schema, base)
        _ -> (Nothing, name)

compareSqlTables :: [SSS.Statement] -> [SSS.Statement] -> AllTableChanges
compareSqlTables oldTables newTables =
    let oldMap = [(getBaseName (getTableName s), s) | s <- oldTables, isCreateTable s]
        newMap = [(getBaseName (getTableName s), s) | s <- newTables, isCreateTable s]

        added = [s | (name, s) <- newMap, name `notElem` map fst oldMap]
        removed = [TableRemoved name | (name, s) <- oldMap, name `notElem` map fst newMap]

        modified = catMaybes $ fmap (\(newName, newTable) -> compareTable newName (lookup newName oldMap) newTable) newMap
    in AllTableChanges
        { tablesAdded = added
        , tablesRemoved = removed
        , tablesModified = modified
        }
  where
    getBaseName = snd . parseTableName

    isCreateTable (SSS.CreateTable _ _ _) = True
    isCreateTable _ = False

    compareTable :: String -> Maybe SSS.Statement -> SSS.Statement -> Maybe TableModified
    compareTable _ Nothing _ = Nothing
    compareTable name (Just oldTable) newTable =
        let (added, removed, modifiedCols) = compareColumns (getTableColumns oldTable) (getTableColumns newTable)
        in if null added && null removed && null modifiedCols
            then Nothing
            else Just TableModified
                { tableName = name
                , columnsAdded = added
                , columnsRemoved = removed
                , columnsModified = modifiedCols
                }

compareColumns :: [SSS.ColumnDef] -> [SSS.ColumnDef] -> ([SSS.ColumnDef], [ColumnRemoved], [ColumnModified])
compareColumns oldCols newCols =
    let oldMap = [(columnName c, c) | c <- oldCols]
        newMap = [(columnName c, c) | c <- newCols]

        added = [c | (name, c) <- newMap, name `notElem` map fst oldMap]
        removed = [ColumnRemoved (columnName c) | (name, c) <- oldMap, name `notElem` map fst newMap]

        modified = catMaybes $ fmap (\(newName, newCol) -> compareColumn newName (lookup newName oldMap) newCol) newMap
    in (added, removed, modified)
  where
    compareColumn :: String -> Maybe SSS.ColumnDef -> SSS.ColumnDef -> Maybe ColumnModified
    compareColumn _ Nothing _ = Nothing
    compareColumn name (Just oldCol) newCol =
                if oldCol /= newCol
                    then Just $ ColumnModified name oldCol newCol
                else Nothing

compareEnums :: [EnumDef] -> [EnumDef] -> EnumChanges
compareEnums oldEnums newEnums =
    let oldMap = [(enumName e, e) | e <- oldEnums]
        newMap = [(enumName e, e) | e <- newEnums]
        added = [e | (name, e) <- newMap, name `notElem` map fst oldMap]
        removed = [EnumRemoved name | (name, _) <- oldMap, name `notElem` map fst newMap]
        modified = mapMaybe (compareEnum oldMap) newMap
    in EnumChanges
        { enumsAdded = added
        , enumsRemoved = removed
        , enumsModified = modified
        }
  where
    compareEnum :: [(String, EnumDef)] -> (String, EnumDef) -> Maybe EnumModified
    compareEnum oldMap (name, newEnum) =
        case lookup name oldMap of
            Nothing -> Nothing
            Just oldEnum ->
                let oldVals = enumValues oldEnum
                    newVals = enumValues newEnum
                    addedVals = [v | v <- newVals, v `notElem` oldVals]
                    removedVals = [v | v <- oldVals, v `notElem` newVals]
                in if not (null addedVals && null removedVals)
                then Just EnumModified
                    { modifiedEnumName = name
                    , enumsValuesAdded = addedVals
                    , enumsValuesRemoved = removedVals
                    }
                else Nothing

processSqlFiles :: [(FilePath, String)] -> [(FilePath, String)] -> IO (AllTableChanges, EnumChanges, [ErrorMessage])
processSqlFiles oldFiles newFiles =
    let (oldTables, oldEnums, oldErrors) = parseSqlFiles (map snd oldFiles)
        (newTables, newEnums, newErrors) = parseSqlFiles (map snd newFiles)
    in return (compareSqlTables oldTables newTables, compareEnums oldEnums newEnums, oldErrors ++ newErrors)

-- Table helpers
getTableColumns :: SSS.Statement -> [SSS.ColumnDef]
getTableColumns (SSS.CreateTable _ elems _) =
    [c | SSS.TableColumnDef c <- elems]
getTableColumns _ = []

getTablePK :: SSS.Statement -> Maybe [String]
getTablePK (SSS.CreateTable _ elems _) =
    let pkConstraints = [map nameToString ns | SSS.TableConstraintDef _ (SSS.TablePrimaryKeyConstraint ns) <- elems]
        colPKs = [nameToString n | SSS.TableColumnDef (SSS.ColumnDef n _ cs) <- elems, any isColPK cs]
    in listToMaybe (pkConstraints ++ [colPKs])
  where
    isColPK (SSS.ColConstraintDef _ (SSS.ColPrimaryKeyConstraint _)) = True
    isColPK _ = False
getTablePK _ = Nothing

getTableFKs :: SSS.Statement -> [(String, String, String)]
getTableFKs (SSS.CreateTable _ elems _) = concatMap extractFK elems
  where
    extractFK (SSS.TableConstraintDef _ (SSS.TableReferencesConstraint localCols refTable mRefCols _ _ _)) =
        case mRefCols of
            Just refCols -> zip3 (map nameToString localCols) (repeat (namesToString refTable)) (map nameToString refCols)
            Nothing -> []
    extractFK (SSS.TableColumnDef (SSS.ColumnDef n _ cs)) =
        [(nameToString n, namesToString refTable, nameToString refCol)
        | SSS.ColConstraintDef _ (SSS.ColReferencesConstraint refTable (Just refCol) _ _ _) <- cs]
    extractFK _ = []
getTableFKs _ = []

getTableName :: SSS.Statement -> String
getTableName (SSS.CreateTable names _ _) = namesToString names
getTableName _ = ""

nameToString :: SSS.Name -> String
nameToString (SSS.Name _ base) = T.unpack base

namesToString :: [SSS.Name] -> String
namesToString = intercalate "." . map nameToString

-- Column helpers
columnName :: SSS.ColumnDef -> String
columnName (SSS.ColumnDef n _ _) = nameToString n

columnType :: SSS.ColumnDef -> String
columnType (SSS.ColumnDef _ mType _) = map toLower $ maybe "UNKNOWN" typeNameToString mType

columnNullable :: SSS.ColumnDef -> Bool
columnNullable (SSS.ColumnDef _ _ constraints) =
    not $ any isNotNull constraints
  where
    isNotNull (SSS.ColConstraintDef _ SSS.ColNotNullConstraint) = True
    isNotNull _ = False

compareColumnTypes :: SSS.ColumnDef -> SSS.ColumnDef -> (Bool, String, String)
compareColumnTypes col1 col2 =
    let col1Type = columnType col1
        col2Type = columnType col2
        sameType =
            case (col1Type, col2Type) of
                ("text", varchar)  | isPrefixOf "varchar" varchar  -> True
                (varchar, "text")  | isPrefixOf "varchar" varchar -> True
                ("centi", decimal) | isPrefixOf "decimal" decimal -> True
                (decimal, "centi") | isPrefixOf "decimal" decimal -> True
                (col1Type, col2Type) -> col1Type == col2Type
    in (sameType, col1Type, col2Type)