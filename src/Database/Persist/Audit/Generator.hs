{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module Database.Persist.Audit.Generator where

import           Data.Monoid ((<>))
import           Data.Char 
import           Data.Text (Text)
import qualified Data.Text as T

import           Database.Persist.Audit.Types



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
-- | can be deleted without affecting the audit models. This is a work around in case the 
-- | original models and the audit models are stored in different databases.
-- | Persist cannot handle keys across SQL and Mongo.
data ForeignKeyType = OriginalKey   -- | Default setting. Link the ids as the original type with a "noreference" tag.
                    | MongoKeyInSQL -- | Store Mongo Key as a ByteString in SQL.
                    | SQLKeyInMongo -- | Store SQL Key as an Int64 in Mongo.
  deriving (Eq,Read,Show)

-- | Settings that the author assumed would be most common.
defaultSettings :: AuditGeneratorSettings
defaultSettings =  AuditGeneratorSettings 2 "Audit" True False False OriginalKey


-- | Convert a list of 'TopLevel' to a list of Audit Models in 'Text'.
generateAuditModels :: AuditGeneratorSettings -> PersistModelFile -> Text
generateAuditModels settings = T.concat . (map $ (flip T.append "\n") . (printTopLevel settings))

-- | Select the correct type from Audit Model to original Model. Used for cross database.
-- | 'fst' is the type and 'snd' is the original type in comment for if using cross database.
printForeignKey :: ForeignKeyType -> Text -> (Text, Text)
printForeignKey OriginalKey   entityName = (entityName <> " noreference", "")
printForeignKey MongoKeyInSQL entityName = ("ByteString", " -- " <> entityName)
printForeignKey SQLKeyInMongo entityName = ("Int64"     , " -- " <> entityName)


-- | Convert a 'TopLevel' to an Audit Model, white space or comment in 'Text'.
printTopLevel :: AuditGeneratorSettings -> PersistModelFilePiece -> Text
printTopLevel settings (PersistModelFileEntity e) = (_getEntityName e <> auditTag settings <> jsonOption <> sqlOption <> "\n")
                                             <> (T.concat $ map (printEntityChild settings) $ _getEntityChildren e)
                                             <> (T.pack $ replicate (childSpacing settings) ' ') <> "originalId " <> foreignKey <> foreignKeyComment <> "\n"
                                             <> (T.pack $ replicate (childSpacing settings) ' ') <> "auditAction AuditAction\n"
                                             <> (T.pack $ replicate (childSpacing settings) ' ') <> "editedBy Text\n"
                                             <> (T.pack $ replicate (childSpacing settings) ' ') <> "editedOn UTCTime\n"
  where
    jsonOption = 
      if _isEntityDeriveJson e then " " <> "json" else  ""
    sqlOption =
      case _getEntitySqlTable e of
        Just s  -> " sql=" <> s  
        Nothing -> ""

    (foreignKey,foreignKeyComment) = printForeignKey (foreignKeyType settings) (_getEntityName e <> "Id")

printTopLevel settings (PersistModelFileComment c) = 
  if keepComments settings then _getComment c else ""

printTopLevel settings (PersistModelFileWhiteSpace w) = 
  if keepSpacing settings then _getWhiteSpace w else ""


-- | Convert an 'EntityChild' to a piece of an Audit Model in 'Text'.
-- | It does not generate anything for EntityUnique, EntityPrimary or EntityForeign
-- | because Audits do not need to be unique, they will have an automatically produced Key
-- | and should not have any foreign keys connecting back to the original model.
printEntityChild :: AuditGeneratorSettings -> EntityChild -> Text
printEntityChild settings (EntityChildEntityField ef) = "  " <> entityFieldName <> " "
                                                     <> entityFieldType
                                                     <> entityDefault
                                                     <> sqlRow
                                                     <> sqlType
                                                     <> maxLen
                                                     <> foreignKeyComment'
                                                     <> "\n"
  where
    entityFieldName = _getEntityFieldName ef
    eft = _getEntityFieldType ef
    eftText = _getEntityFieldTypeText eft
    (foreignKey,foreignKeyComment) = printForeignKey (foreignKeyType settings) eftText

    entityFieldType = 
      case _getEntityFieldStrictness eft of 
        Strict -> ""
        ExplicitStrict -> "!"
        Lazy -> "~"
      <> maybeLeftBracket
      <> entityType
      <> maybeRightBracket
      <> if _isEntityFieldTypeMaybe eft then " Maybe" else ""
    
    entityDefault = 
      case _getEntityFieldDefault ef of
        Just d -> " default=" <> d
        Nothing -> ""

    sqlRow =
      case _getEntityFieldSqlRow ef of
        Just sr -> " sql=" <> sr
        Nothing -> ""

    sqlType =
      case _getEntityFieldSqlType ef of
        Just st -> " sqltype=" <> st
        Nothing -> ""

    maxLen =
      case _getEntityFieldMaxLen ef of
        Just ml -> " maxlen=" <> (T.pack . show $ ml)
        Nothing -> ""

    maybeLeftBracket = if _isEntityFieldTypeList eft then "[" else ""
    maybeRightBracket = if _isEntityFieldTypeList eft  then "]" else ""
    entityType = if stringEndsInId . T.unpack $ eftText then foreignKey else eftText
    foreignKeyComment' = if stringEndsInId . T.unpack $ eftText then foreignKeyComment else ""

printEntityChild _ (EntityChildEntityDerive  d) = "  " <> "deriving" <> " " <> (T.intercalate " " (_getEntityDeriveTypes d)) <> "\n"
printEntityChild _ (EntityChildEntityUnique  _) = ""
printEntityChild _ (EntityChildEntityPrimary _) = ""
printEntityChild _ (EntityChildEntityForeign _) = ""

printEntityChild settings (EntityChildComment c) = if keepComments settings then _getComment c else ""
printEntityChild settings (EntityChildWhiteSpace w) = if keepSpacing settings then _getWhiteSpace w else ""


-- | Convert a list of 'TopLevel' to a to a list of 'ToAudit' in 'Text'.
generateToAuditInstances ::  AuditGeneratorSettings -> PersistModelFile -> Text
generateToAuditInstances settings = T.concat . (map $ printToAuditInstance settings)

-- | Convert 'TopLevel' to an instance of 'ToAudit' in 'Text'.
printToAuditInstance :: AuditGeneratorSettings -> PersistModelFilePiece -> Text
printToAuditInstance settings (PersistModelFileEntity e) =  "instance ToAudit " <> entityName <> " where\n"
                                    <> "  type AuditResult " <> entityName <> " = " <> auditEntityName <> "\n"
                                    <> "  toAudit v k auditAction editedBy editedOn = " <> auditEntityName <> "\n"
                                    <> (T.concat $ map (printModelAccessor settings entityName) entityChildren)
                                    <> "    (" <> ifForeignKeyAlternate <> "k) auditAction editedBy editedOn\n\n"
  where
    entityName = _getEntityName e
    auditEntityName = entityName <> (auditTag settings)
    entityChildren = _getEntityChildren e
    ifForeignKeyAlternate = printIfForeignKeyAlternate (foreignKeyType settings) "Id"

printToAuditInstance _ _ = ""

-- encodeUtf8

-- | Convert 'EntityChild' to a Model accessor.
printModelAccessor :: AuditGeneratorSettings -> Text -> EntityChild -> Text
printModelAccessor settings entityName (EntityChildEntityField ef) = "    ("
                                                        <> ifForeignKeyAlternate
                                                        <> (T.pack . firstLetterToLowerCase . T.unpack $ entityName) 
                                                        <> (T.pack . firstLetterToUpperCase . T.unpack $ _getEntityFieldName ef)
                                                        <> " v)\n"
  where
    entityFieldType = _getEntityFieldType ef
    ifForeignKeyAlternate = printIfForeignKeyAlternate2 (foreignKeyType settings) (_getEntityFieldTypeText entityFieldType) entityFieldType

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

-- | If 'fst' True then prefix the function with 'fmap'
printEntityFieldTypeFunctionConnector :: EntityFieldType -> (Bool, Text)
printEntityFieldTypeFunctionConnector eft = 
  if _isEntityFieldTypeMaybe eft && _isEntityFieldTypeList eft 
    then (True, " <$>") 
    else 
      if _isEntityFieldTypeMaybe eft || _isEntityFieldTypeList eft 
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
