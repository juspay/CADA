{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables, OverloadedRecordDot, DuplicateRecordFields #-}
module Validation where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.List (find, intercalate, isPrefixOf, isInfixOf, stripPrefix, nub)
import Data.Functor (($>))
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified SqlDiff
import qualified Language.SQL.SimpleSQL.Syntax as SSS

data ValidationError = ValidationError
    { veTypeName :: String
    , veIssue :: String
    , veDetails :: String
    } deriving (Show, Eq, Generic)

instance ToJSON ValidationError where
    toJSON = genericToJSON (SqlDiff.recordOptions (Just "ve"))

data ValidationResult = ValidationResult
    { vrErrors :: [ValidationError]
    , vrPassed :: Bool
    } deriving (Show, Eq, Generic)

instance ToJSON ValidationResult where
    toJSON = genericToJSON (SqlDiff.recordOptions (Just "vr"))

data HsCodeChangeEntry = HsCodeChangeEntry
    { tcModule :: String
    , tcName :: String
    , tcOldCode :: Maybe String
    , tcNewCode :: Maybe String
    , tcChangeType :: ChangeType
    }deriving (Show, Eq, Generic)

data ChangeType = Added | Deleted | Modified
    deriving (Show, Eq, Generic)

data HsColumn = HsColumn
    { colName :: String
    , colType :: String
    , colNullable :: Bool
    } deriving (Show, Eq)

hsColumnToSSS :: HsColumn -> SSS.ColumnDef
hsColumnToSSS (HsColumn name typ nullable) =
    SSS.ColumnDef (SSS.Name Nothing (T.pack name))
        (Just (SSS.TypeName [SSS.Name Nothing (T.pack typ)]))
        (if nullable then [] else [SSS.ColConstraintDef Nothing SSS.ColNotNullConstraint])

data TypeChangesFile = TypeChangesFile
    { codeAdded :: [CodeAddedOrDeleted]
    , codeDeleted :: [CodeAddedOrDeleted]
    , codeModified :: [CodeModified]
    } deriving (Show, Generic)

data CodeAddedOrDeleted = CodeAddedOrDeleted
    { codeModule :: String
    , codeIdentifier :: String
    , codeTxt :: String
    } deriving (Show, Generic)

data CodeModified = CodeModified
    { codeModule :: String
    , codeIdentifier :: String
    , oldCode :: String
    , newCode :: String
    } deriving (Show, Generic)

instance FromJSON CodeAddedOrDeleted where
    parseJSON = withObject "CodeAddedOrDeleted" $ \v -> CodeAddedOrDeleted
        <$> v .: "module"
        <*> v .: "name"
        <*> v .: "code"

instance FromJSON CodeModified where
    parseJSON = withObject "CodeModified" $ \v -> CodeModified
        <$> v .: "module"
        <*> v .: "name"
        <*> v .: "oldCode"
        <*> v .: "newCode"

instance FromJSON TypeChangesFile where
    parseJSON = genericParseJSON (SqlDiff.recordOptions (Just "code"))

flattenTypeChanges :: TypeChangesFile -> [HsCodeChangeEntry] -- nested structure to flat list of HsCodeChangeEntry
flattenTypeChanges typeChangeFile =
    let added = map (\codeAdded -> HsCodeChangeEntry codeAdded.codeModule codeAdded.codeIdentifier Nothing (Just codeAdded.codeTxt) Added) typeChangeFile.codeAdded
        deleted = map (\codeRemoved -> HsCodeChangeEntry codeRemoved.codeModule codeRemoved.codeIdentifier Nothing Nothing Deleted) typeChangeFile.codeDeleted
        modified = map (\codeModified -> HsCodeChangeEntry codeModified.codeModule codeModified.codeIdentifier (Just codeModified.oldCode) (Just codeModified.newCode) Modified) typeChangeFile.codeModified
    in added ++ deleted ++ modified

isDbTypeModule :: String -> Bool
isDbTypeModule modName = "Types.DB." `isInfixOf` modName

isBeamTableType :: String -> Bool
isBeamTableType name = not (null name) && last name == 'T'

isFromFieldInstance :: String -> Bool
isFromFieldInstance modName = "FromField" `isInfixOf` modName

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

extractColumnDefs :: String -> [HsColumn]
extractColumnDefs input =
    case parse recordFields "" input of
        Left _     -> []
        Right cols -> nub cols

recordFields :: Parser [HsColumn]
recordFields = do
    _ <- manyTill anySingle (lookAhead (char '{'))
    between (symbol "{") (symbol "}") (sepBy fieldDef (symbol ","))

fieldDef :: Parser HsColumn
fieldDef = do
    _ <- optional (symbol "!" <|> symbol "~")
    name <- fieldName
    _ <- symbol "::"
    (typ, nullable) <- fieldType
    return $ HsColumn (camelTo2 '_' name) (SqlDiff.getTextFromQualifiedText typ) nullable

fieldName :: Parser String
fieldName = lexeme $ (:) <$> lowerChar <*> many (alphaNumChar <|> char '_' <|> char '\'')

fieldType :: Parser (String, Bool)
fieldType = choice [ try columnarWrapper, try cWrapper, try maybeType, bareType ]

columnarWrapper, cWrapper :: Parser (String, Bool)
columnarWrapper = qualifiedSymbol "Columnar" >> symbol "f" >> innerType
cWrapper        = qualifiedSymbol "C"        >> symbol "f" >> innerType

qualifiedSymbol :: String -> Parser String
qualifiedSymbol sym = try $ do
    _ <- optional $ try $ some letterChar <* char '.'
    symbol sym

maybeType, bareType :: Parser (String, Bool)
maybeType = symbol "Maybe" >> typeExpr >>= \t -> return (t, True)
bareType  = typeExpr >>= \t -> return (t, False)

innerType :: Parser (String, Bool)
innerType = choice
    [ try $ between (symbol "(") (symbol ")") $ do
          _ <- symbol "Maybe"
          t <- typeExpr
          return (t, True)
    , try maybeType
    , bareType
    ]

typeExpr :: Parser String
typeExpr = intercalate " " <$> some typeAtom

typeAtom :: Parser String
typeAtom = choice
    [ try $ between (symbol "(") (symbol ")") tupleOrParenType
    , try $ (\mt -> "[" ++ maybe "" id mt ++ "]") <$> between (symbol "[") (symbol "]") (optional typeExpr)
    , lexeme $ some (noneOf ("(),[]{} " :: String))
    ]

tupleOrParenType :: Parser String
tupleOrParenType = do
    elems <- sepBy typeExpr (symbol ",")
    return $ "(" ++ intercalate ", " elems ++ ")"

---------------------- Comparing Tables ----------------------
compareAllTableChanges :: SqlDiff.AllTableChanges -> SqlDiff.AllTableChanges -> ValidationResult -- validating changes2
compareAllTableChanges changes1 changes2 =
    let errors = concatMap (validateHsTableAdded changes1.tablesAdded) changes2.tablesAdded
              ++ concatMap (validateHsTableRemoved changes1.tablesRemoved) changes2.tablesRemoved
              ++ concatMap (validateHsTableModified changes1.tablesModified) changes2.tablesModified
    in ValidationResult errors (null errors)

validateHsTableAdded :: [SSS.Statement] -> SSS.Statement -> [ValidationError]
validateHsTableAdded allTablesAddedInChange1 tableAddedInChange2 =
    let t2Name = SqlDiff.getTableName tableAddedInChange2
    in case find (\s -> SqlDiff.getTableName s == t2Name) allTablesAddedInChange1 of
        Just tableAddedInChange1 -> concatMap (validateColumnAdded t2Name (SqlDiff.getTableColumns tableAddedInChange1)) (SqlDiff.getTableColumns tableAddedInChange2)
        Nothing -> [ValidationError
            { veTypeName = t2Name
            , veIssue = "MISSING_TABLE_ADDITION"
            , veDetails = "Table not added: " ++ t2Name
            }]

validateHsTableRemoved :: [SqlDiff.TableRemoved] -> SqlDiff.TableRemoved -> [ValidationError]
validateHsTableRemoved allTablesRemovedInChange1 tableRemovedInChange2 =
    case find (\t -> t.tableName == tableRemovedInChange2.tableName) allTablesRemovedInChange1 of
        Just _tableRemovedInChange1 -> []
        Nothing -> [ValidationError
            { veTypeName = tableRemovedInChange2.tableName
            , veIssue = "MISSING_TABLE_REMOVAL"
            , veDetails = "Table not removed: " ++ tableRemovedInChange2.tableName
            }]

validateHsTableModified :: [SqlDiff.TableModified] -> SqlDiff.TableModified -> [ValidationError]
validateHsTableModified allTablesModifiedInChange1 tableModifiedInChange2 =
    case find (\t -> t.tableName == tableModifiedInChange2.tableName) allTablesModifiedInChange1 of
        Just tableModifiedInChange1 ->
            concatMap (validateColumnAdded tableModifiedInChange2.tableName tableModifiedInChange1.columnsAdded) tableModifiedInChange2.columnsAdded
            ++ concatMap (validateColumnRemoved tableModifiedInChange2.tableName tableModifiedInChange1.columnsRemoved) tableModifiedInChange2.columnsRemoved
            ++ concatMap (validateColumnModified tableModifiedInChange2.tableName tableModifiedInChange1.columnsModified) tableModifiedInChange2.columnsModified
        Nothing -> [ValidationError
            { veTypeName = tableModifiedInChange2.tableName
            , veIssue = "MISSING_TABLE_MODIFICATION"
            , veDetails = "Table not modified in SQL: " ++ tableModifiedInChange2.tableName
            }]

validateColumnAdded :: String -> [SSS.ColumnDef] -> SSS.ColumnDef -> [ValidationError]
validateColumnAdded tableName table1Columns table2Column =
    let t2Name = SqlDiff.columnName table2Column
    in case find (\c -> SqlDiff.columnName c == t2Name) table1Columns of
        Just table1Column -> do
            let (sameType, table1ColumnType, table2ColumnType) = SqlDiff.compareColumnTypes table1Column table2Column
            [ValidationError tableName "TYPE_MISMATCH" ("Type mismatch for" ++ SqlDiff.columnName table2Column ++ ": " ++ table1ColumnType ++ ", " ++ table2ColumnType) | (not sameType)]
            ++ [ValidationError tableName "NULLABLE_MISMATCH" ("Nullable mismatch for " ++ SqlDiff.columnName table1Column) | (SqlDiff.columnNullable table1Column /= SqlDiff.columnNullable table2Column)]
        Nothing -> [ValidationError tableName "MISSING_COLUMN" ("Column missing " ++ t2Name)]

validateColumnRemoved :: String -> [SqlDiff.ColumnRemoved] -> SqlDiff.ColumnRemoved -> [ValidationError]
validateColumnRemoved tableName table1Columns table2Column =
    case find (\t -> t.colName == table2Column.colName) table1Columns of
        Just _ -> []
        Nothing -> [ValidationError tableName "MISSING_COLUMN_REMOVAL" ("Column not removed in SQL: " ++ table2Column.colName)]

validateColumnModified :: String -> [SqlDiff.ColumnModified] -> SqlDiff.ColumnModified -> [ValidationError]
validateColumnModified tableName table1Columns table2Column =
    case find (\c -> c.colName == table2Column.colName) table1Columns of
        Just table1Column ->
            let t1New = table1Column.new
                t2New = table2Column.new
                (sameType, t1Type, t2Type) = SqlDiff.compareColumnTypes t1New t2New
                nullableMismatch = SqlDiff.columnNullable t1New /= SqlDiff.columnNullable t2New
                typeError = [ValidationError tableName "MODIFIED_TYPE_MISMATCH" ("type mismatch for column " ++ table2Column.colName ++ ": " ++ t1Type ++ ", " ++ t2Type) | not sameType]
                nullableError = [ValidationError tableName "MODIFIED_NULLABLE_MISMATCH" ("Modified nullable mismatch for " ++ table2Column.colName) | nullableMismatch]
            in typeError ++ nullableError
        Nothing -> [ValidationError tableName "MISSING_COLUMN_MODIFICATION" ("Column not modified in SQL: " ++ table2Column.colName)]

---------------------- Comparing Enums ----------------------
compareAllEnumChanges :: SqlDiff.EnumChanges -> SqlDiff.EnumChanges -> ValidationResult
compareAllEnumChanges changes1 changes2 =
    let errors = concatMap (validateHsEnumAdded changes1.enumsAdded) changes2.enumsAdded
              ++ concatMap (validateHsEnumRemoved changes1.enumsRemoved) changes2.enumsRemoved
              ++ concatMap (validateHsEnumModified changes1.enumsModified) changes2.enumsModified
    in ValidationResult errors (null errors)

validateHsEnumAdded :: [SqlDiff.EnumDef] -> SqlDiff.EnumDef -> [ValidationError]
validateHsEnumAdded allEnumsAddedInChange1 enumAddedInChange2 =
    let e2Name = enumAddedInChange2.enumName
    in case find (\e -> e.enumName == e2Name) allEnumsAddedInChange1 of
        Just enumAddedInChange1 ->
            concatMap (validateEnumValue e2Name enumAddedInChange1.enumValues "MISSING_ENUM_VALUE_ADDITION") enumAddedInChange2.enumValues
        Nothing -> [ValidationError
            { veTypeName = e2Name
            , veIssue = "MISSING_ENUM_TYPE_ADDITION"
            , veDetails = "Enum not added: " ++ e2Name
            }]

validateHsEnumRemoved :: [SqlDiff.EnumRemoved] -> SqlDiff.EnumRemoved -> [ValidationError]
validateHsEnumRemoved allEnumsRemovedInChange1 enumRemovedInChange2 =
    case find (\e -> e.removedEnumName == enumRemovedInChange2.removedEnumName) allEnumsRemovedInChange1 of
        Just _ -> []
        Nothing -> [ValidationError
            { veTypeName = enumRemovedInChange2.removedEnumName
            , veIssue = "MISSING_ENUM_TYPE_REMOVAL"
            , veDetails = "Enum not removed: " ++ enumRemovedInChange2.removedEnumName
            }]

validateHsEnumModified :: [SqlDiff.EnumModified] -> SqlDiff.EnumModified -> [ValidationError]
validateHsEnumModified allEnumsModifiedInChange1 enumModifiedInChange2 =
    case find (\e -> e.modifiedEnumName == enumModifiedInChange2.modifiedEnumName) allEnumsModifiedInChange1 of
        Just enumModifiedInChange1 ->
            let addedErrors = concatMap (validateEnumValue enumModifiedInChange2.modifiedEnumName enumModifiedInChange1.enumsValuesAdded "MISSING_ENUM_VALUE_ADDITION") enumModifiedInChange2.enumsValuesAdded
                removedErrors = concatMap (validateEnumValue enumModifiedInChange2.modifiedEnumName enumModifiedInChange1.enumsValuesRemoved "MISSING_ENUM_VALUE_REMOVAL") enumModifiedInChange2.enumsValuesRemoved
            in addedErrors ++ removedErrors
        Nothing -> [ValidationError
            { veTypeName = enumModifiedInChange2.modifiedEnumName
            , veIssue = "MISSING_ENUM_MODIFICATION"
            , veDetails = "Enum not modified in SQL: " ++ enumModifiedInChange2.modifiedEnumName
            }]

validateEnumValue :: String -> [String] -> String -> String -> [ValidationError]
validateEnumValue enumName enum1Values errorTag enum2Value =
    if enum2Value `elem` enum1Values
        then []
        else [ValidationError enumName errorTag ("Enum value missing: " ++ enum2Value)]

parseEnumValues :: String -> [String]
parseEnumValues input =
    case parse allMatches "" input of
        Left _   -> []
        Right vs -> nub vs
  where
    allMatches :: Parser [String]
    allMatches = catMaybes <$> many matchOrSkip

    matchOrSkip :: Parser (Maybe String)
    matchOrSkip = try enumMatch <|> (anySingle $> Nothing)

    enumMatch :: Parser (Maybe String)
    enumMatch = do
        _ <- optional $ try (string "Just" >> many spaceChar)
        val <- between (char '"') (char '"') (many (noneOf ['"']))
        _ <- many spaceChar
        _ <- string "->"
        return (if null val then Nothing else Just val)

hsCodeChangesToAllTableChanges :: [HsCodeChangeEntry] -> SqlDiff.AllTableChanges
hsCodeChangesToAllTableChanges hsCodeChanges = do
    let dbTypeChanges = filter (\tc -> isDbTypeModule tc.tcModule && isBeamTableType tc.tcName) hsCodeChanges

        tablesAdded = catMaybes [toTableDef hsCodeChange | hsCodeChange <- dbTypeChanges, hsCodeChange.tcChangeType == Added]
        tablesRemoved = [SqlDiff.TableRemoved (getTableName hsCodeChange) | hsCodeChange <- dbTypeChanges, hsCodeChange.tcChangeType == Deleted]
        tablesModified = catMaybes [toTableModified hsCodeChange | hsCodeChange <-dbTypeChanges, hsCodeChange.tcChangeType == Modified]
    SqlDiff.AllTableChanges tablesAdded tablesRemoved tablesModified
  where
    toTableDef :: HsCodeChangeEntry -> Maybe SSS.Statement
    toTableDef hsCodeChange = do
        code <- hsCodeChange.tcNewCode
        let tableName = getTableName hsCodeChange
            columns = map hsColumnToSSS (extractColumnDefs code)
        return $ SSS.CreateTable [SSS.Name Nothing (T.pack tableName)] (map SSS.TableColumnDef columns) False

    toTableModified :: HsCodeChangeEntry -> Maybe SqlDiff.TableModified
    toTableModified hsCodeChange =
        let tableName = getTableName hsCodeChange
            (addedCols, removedCols, modifiedCols) = getAddedRemovedAndModifiedFields tableName hsCodeChange.tcOldCode hsCodeChange.tcNewCode
        in Just $ SqlDiff.TableModified tableName addedCols removedCols modifiedCols

    getTableName :: HsCodeChangeEntry -> String
    getTableName hsCodeChange =
        let name = hsCodeChange.tcName
            withoutT = if not (null name) && last name == 'T'
                    then init name
                    else name
        in camelTo2 '_' withoutT

    getAddedRemovedAndModifiedFields :: String -> Maybe String -> Maybe String -> ([SSS.ColumnDef], [SqlDiff.ColumnRemoved], [SqlDiff.ColumnModified])
    getAddedRemovedAndModifiedFields tableName mOldCode mNewCode =
        let oldFields = map hsColumnToSSS $ maybe [] extractColumnDefs mOldCode
            newFields = map hsColumnToSSS $ maybe [] extractColumnDefs mNewCode
        in SqlDiff.compareColumns oldFields newFields

hsCodeChangesToEnumChanges :: [HsCodeChangeEntry] -> SqlDiff.EnumChanges
hsCodeChangesToEnumChanges enumEntries =
    let dbEnumChanges = filter (\tc -> isDbTypeModule tc.tcModule && isFromFieldInstance tc.tcName) enumEntries

        added = catMaybes [toEnumDef e | e <- dbEnumChanges, e.tcChangeType == Added]
        removed = [SqlDiff.EnumRemoved (getEnumName e) | e <- dbEnumChanges, e.tcChangeType == Deleted]
        modified = catMaybes [toEnumModified e | e <- dbEnumChanges, e.tcChangeType == Modified]
    in SqlDiff.EnumChanges added removed modified
  where
    toEnumDef :: HsCodeChangeEntry -> Maybe SqlDiff.EnumDef
    toEnumDef e = do
        code <- e.tcNewCode
        let values = parseEnumValues code
        return $ SqlDiff.EnumDef (getEnumName e) values

    toEnumModified :: HsCodeChangeEntry -> Maybe SqlDiff.EnumModified
    toEnumModified e = do
        oldCode <- e.tcOldCode
        newCode <- e.tcNewCode
        let oldVals = parseEnumValues oldCode
            newVals = parseEnumValues newCode
            addedVals = [v | v <- newVals, v `notElem` oldVals]
            removedVals = [v | v <- oldVals, v `notElem` newVals]
        if null addedVals && null removedVals
            then Nothing
            else Just $ SqlDiff.EnumModified (getEnumName e) addedVals removedVals

    getEnumName :: HsCodeChangeEntry -> String
    getEnumName hsCodeChangeEntry = camelTo2 '_' $
        case stripPrefix "FromField" (SqlDiff.trim hsCodeChangeEntry.tcName) of
            Just rest -> SqlDiff.trim rest
            Nothing -> hsCodeChangeEntry.tcName

writeHsTableDelta :: TypeChangesFile -> IO SqlDiff.AllTableChanges
writeHsTableDelta typeChangesFile = do
    let hsTableDelta = hsCodeChangesToAllTableChanges $ flattenTypeChanges typeChangesFile
    writeFile "hs_table_delta.json" (BLU.toString . encodePretty $ hsTableDelta)
    pure hsTableDelta

writeHsEnumDelta :: TypeChangesFile -> IO SqlDiff.EnumChanges
writeHsEnumDelta instanceChangesFile = do
    let hsEnumDelta = hsCodeChangesToEnumChanges $ flattenTypeChanges instanceChangesFile
    writeFile "hs_enum_delta.json" (BLU.toString . encodePretty $ hsEnumDelta)
    pure hsEnumDelta