{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Database.Persist.Audit.Generator where

import           Data.Monoid ((<>))
import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T

import           Database.Persist.Audit.Types
import           Database.Persist.Syntax.Types


-- | Five options for generating Audit Models and ToAudit Instances.
data AuditGeneratorSettings = AuditGeneratorSettings {
  childSpacing     :: Int   -- ^ The number of spaces to add for all items that appear under an EntityName.
, auditTag         :: Text  -- ^ The tag that will be added to the original model name in the generated audit models. If 'auditTag' is "History" then "User" will become "UserHistory".
, keepEntityDerive :: Bool  -- ^ If 'True', the generated Audit Models will maintain the same derived Type Classes as the original file.
, keepComments     :: Bool  -- ^ If 'True', the generated Audit Models will maintain the same comments as the original file.
, keepSpacing      :: Bool  -- ^ If 'True', the generated Audit Models will maintain the same spacing as the original file.
, foreignKeyType   :: ForeignKeyType -- ^ Foreign Keys can be the original type, ByteString or Int64.
} deriving (Eq,Read,Show)


-- | All foreign keys are kept in the audit models but derefenced so the original models
-- can be deleted without affecting the audit models. This is a work around in case the
-- original models and the audit models are stored in different databases.
-- Persist cannot handle keys across SQL and Mongo. A Mongo Key stored in SQL
-- will be stored as a 'ByteString'. A SQL Key stored in Mongo will be an 'Int64'.
data ForeignKeyType
  -- | Default setting. Link the ids as the original type with a "noreference" tag.
  = OriginalKey
  -- | Store Mongo Key as a ByteString in SQL.
  | MongoKeyInSQL
  -- | Store SQL Key as an Int64 in Mongo.
  | SQLKeyInMongo
  deriving (Eq,Read,Show)

-- | Settings that the author assumed would be most common.
defaultSettings :: AuditGeneratorSettings
defaultSettings =  AuditGeneratorSettings 2 "Audit" True False False OriginalKey


-- | Convert a list of 'TopLevel' to a list of Audit Models in 'Text'.
generateAuditModels :: AuditGeneratorSettings -> ModelsFile -> Text
generateAuditModels settings = T.concat . (map $ (flip T.append "\n") . (printTopLevel settings))

-- | Select the correct type from Audit Model to original Model. Used for cross database.
-- | 'fst' is the type and 'snd' is the original type in comment for if using cross database.
printForeignKey :: ForeignKeyType -> Text -> (Text, Text)
printForeignKey OriginalKey   entityName = (entityName <> " noreference", "")
printForeignKey MongoKeyInSQL entityName = ("ByteString", " -- " <> entityName)
printForeignKey SQLKeyInMongo entityName = ("Int64"     , " -- " <> entityName)


-- | Convert a 'TopLevel' to an Audit Model, white space or comment in 'Text'.
printTopLevel :: AuditGeneratorSettings -> ModelsFilePiece -> Text
printTopLevel settings (ModelsFileEntity e) = (entityName e <> auditTag settings <> jsonOption <> sqlOption <> "\n")
                                             <> (T.concat $ map (printEntityChild settings) $ entityChildren e)
                                             <> (T.pack $ replicate (childSpacing settings) ' ') <> "originalId " <> foreignKey <> foreignKeyComment <> "\n"
                                             <> (T.pack $ replicate (childSpacing settings) ' ') <> "auditAction AuditAction\n"
                                             <> (T.pack $ replicate (childSpacing settings) ' ') <> "editedBy Text\n"
                                             <> (T.pack $ replicate (childSpacing settings) ' ') <> "editedOn UTCTime\n"
  where
    jsonOption =
      if entityDeriveJson e then " " <> "json" else  ""
    sqlOption =
      case entitySqlTable e of
        Just s  -> " sql=" <> s
        Nothing -> ""

    (foreignKey,foreignKeyComment) = printForeignKey (foreignKeyType settings) (entityName e <> "Id")

printTopLevel settings (ModelsFileComment c) =
  if keepComments settings then comment c else ""

printTopLevel settings (ModelsFileWhiteSpace w) =
  if keepSpacing settings then whiteSpace w else ""


-- | Convert an 'EntityChild' to a piece of an Audit Model in 'Text'.
-- It does not generate anything for EntityUnique, EntityPrimary or EntityForeign
-- because Audits do not need to be unique, they will have an automatically produced Key
-- and should not have any foreign keys connecting back to the original model.
printEntityChild :: AuditGeneratorSettings -> EntityChild -> Text
printEntityChild settings (EntityChildEntityField ef) = "  " <> entityFieldNam <> " "
                                                     <> entityFieldTyp
                                                     <> entityDefault
                                                     <> sqlRow
                                                     <> sqlType
                                                     <> maxLen
                                                     <> foreignKeyComment'
                                                     <> "\n"
  where
    entityFieldNam = entityFieldName ef
    eft = entityFieldType ef
    eftText = entityFieldTypeText eft
    (foreignKey,foreignKeyComment) = printForeignKey (foreignKeyType settings) eftText

    entityFieldTyp =
      case entityFieldStrictness eft of
        Strict -> ""
        ExplicitStrict -> "!"
        Lazy -> "~"
      <> maybeLeftBracket
      <> entityType
      <> maybeRightBracket
      <> if entityFieldTypeMaybe eft then " Maybe" else ""

    entityDefault =
      case entityFieldDefault ef of
        Just d -> " default=" <> d
        Nothing -> ""

    sqlRow =
      case entityFieldSqlRow ef of
        Just sr -> " sql=" <> sr
        Nothing -> ""

    sqlType =
      case entityFieldSqlType ef of
        Just st -> " sqltype=" <> st
        Nothing -> ""

    maxLen =
      case entityFieldMaxLen ef of
        Just ml -> " maxlen=" <> (T.pack . show $ ml)
        Nothing -> ""

    maybeLeftBracket = if entityFieldTypeList eft then "[" else ""
    maybeRightBracket = if entityFieldTypeList eft  then "]" else ""
    entityType = if stringEndsInId . T.unpack $ eftText then foreignKey else eftText
    foreignKeyComment' = if stringEndsInId . T.unpack $ eftText then foreignKeyComment else ""

printEntityChild _ (EntityChildEntityDerive  d) = "  " <> "deriving" <> " " <> (T.intercalate " " (entityDeriveTypes d)) <> "\n"
printEntityChild _ (EntityChildEntityUnique  _) = ""
printEntityChild _ (EntityChildEntityPrimary _) = ""
printEntityChild _ (EntityChildEntityForeign _) = ""

printEntityChild settings (EntityChildComment c) = if keepComments settings then comment c else ""
printEntityChild settings (EntityChildWhiteSpace w) = if keepSpacing settings then whiteSpace w else ""


-- | Convert a list of 'TopLevel' to a to a list of 'ToAudit' in 'Text'.
generateToAuditInstances ::  AuditGeneratorSettings -> ModelsFile -> Text
generateToAuditInstances settings = T.concat . (map $ printToAuditInstance settings)

-- | Convert 'TopLevel' to an instance of 'ToAudit' in 'Text'.
printToAuditInstance :: AuditGeneratorSettings -> ModelsFilePiece -> Text
printToAuditInstance settings (ModelsFileEntity e) =  "instance ToAudit " <> entityNam <> " where\n"
                                    <> "  type AuditResult " <> entityNam <> " = " <> auditEntityName <> "\n"
                                    <> "  toAudit v k auditAction editedBy editedOn = " <> auditEntityName <> "\n"
                                    <> (T.concat $ map (printModelAccessor settings entityNam) entityChildre)
                                    <> "    (" <> ifForeignKeyAlternate <> "k) auditAction editedBy editedOn\n\n"
  where
    entityNam = entityName e
    auditEntityName = entityNam <> (auditTag settings)
    entityChildre = entityChildren e
    ifForeignKeyAlternate = printIfForeignKeyAlternate (foreignKeyType settings) "Id"

printToAuditInstance _ _ = ""

-- encodeUtf8

-- | Convert 'EntityChild' to a Model accessor.
printModelAccessor :: AuditGeneratorSettings -> Text -> EntityChild -> Text
printModelAccessor settings entityName (EntityChildEntityField ef) = "    ("
                                                        <> ifForeignKeyAlternate
                                                        <> (T.pack . firstLetterToLowerCase . T.unpack $ entityName)
                                                        <> (T.pack . firstLetterToUpperCase . T.unpack $ entityFieldName ef)
                                                        <> " v)\n"
  where
    entityFieldTyp = entityFieldType ef
    ifForeignKeyAlternate = printIfForeignKeyAlternate2 (foreignKeyType settings) (entityFieldTypeText entityFieldTyp) entityFieldTyp

printModelAccessor _ _ _ = ""


-- | Select the correct function for handling foreign keys.
printIfForeignKeyAlternate :: ForeignKeyType -> Text -> Text
printIfForeignKeyAlternate MongoKeyInSQL entityName =
  case stringEndsInId $ T.unpack entityName of
    False -> ""
    True  -> "mongoKeyToByteString "

printIfForeignKeyAlternate SQLKeyInMongo entityName =
  case stringEndsInId $ T.unpack entityName of
    False -> ""
    True  -> "fromSqlKey "

printIfForeignKeyAlternate _ _ = ""


printIfForeignKeyAlternate2 :: ForeignKeyType -> Text -> EntityFieldType -> Text
printIfForeignKeyAlternate2 MongoKeyInSQL entityName entityFieldType =
  case stringEndsInId $ T.unpack entityName of
    False -> ""
    True  ->
      case needsPrefix of
        False -> "mongoKeyToByteString" <> funcInfix <> " "
        True  -> "fmap mongoKeyToByteString" <> funcInfix <> " "

  where
    (needsPrefix,funcInfix) = printEntityFieldTypeFunctionConnector entityFieldType

printIfForeignKeyAlternate2 SQLKeyInMongo entityName entityFieldType =
  case stringEndsInId $ T.unpack entityName of
    False -> ""
    True  ->
     case needsPrefix of
       False -> "fromSqlKey" <> funcInfix <> " "
       True  -> "fmap fromSqlKey" <> funcInfix <> " "

  where
    (needsPrefix,funcInfix) = printEntityFieldTypeFunctionConnector entityFieldType

printIfForeignKeyAlternate2 _ _ _ = ""

-- | If 'fst' is 'True' then prefix the function with 'fmap'.
printEntityFieldTypeFunctionConnector :: EntityFieldType -> (Bool, Text)
printEntityFieldTypeFunctionConnector eft =
  if entityFieldTypeMaybe eft && entityFieldTypeList eft
    then (True, " <$>")
    else
      if entityFieldTypeMaybe eft || entityFieldTypeList eft
        then (False, " <$>")
        else (False, " $")

-- | Return true if the last two characters are "Id".
stringEndsInId :: String -> Bool
stringEndsInId s = if length s > 1 then hasId $ reverse s else False
  where
    hasId ('d':'I':_) = True
    hasId _ = False

-- | Convert the first letter of a 'String' to the corresponding uppercase letter.
firstLetterToUpperCase :: String -> String
firstLetterToUpperCase (h:r) = toUpper h : r
firstLetterToUpperCase _     = []

-- | Convert the first letter of a 'String' to the corresponsing lowercase letter.
firstLetterToLowerCase :: String -> String
firstLetterToLowerCase (h:r) = toLower h : r
firstLetterToLowerCase _     = []
