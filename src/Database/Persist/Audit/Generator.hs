{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module Database.Persist.Audit.Generator where

import           Data.Monoid ((<>))
import           Data.Char 
import           Data.Text (Text)
import qualified Data.Text as T

import           Database.Persist.Audit.Types


{-
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

printForeignKey :: ForeignKeyType -> Text -> Text
printForeignKey OriginalKey   entityName = entityName <> " noreference"
printForeignKey MongoKeyInSQL entityName = "ByteString" --  -- " <> entityName
printForeignKey SQLKeyInMongo entityName = "Int64" -- -- " <> entityName

-- | Convert a 'TopLevel' to an Audit Model, white space or comment in 'Text'.
printTopLevel :: AuditGeneratorSettings -> PersistModelFilePiece -> Text
printTopLevel settings (PersistModelFileEntity     e) = (_getEntityName e <> auditTag settings <> "\n")
                                             <> (T.concat $ map (printEntityChild settings) $ _getEntityChildren e)
                                             <> (T.pack $ replicate (childSpacing settings) ' ') <> "originalId " <> (printForeignKey (foreignKeyType settings) (_getEntityName e <> "Id")) <> "\n"
                                             <> (T.pack $ replicate (childSpacing settings) ' ') <> "auditAction AuditAction\n"
                                             <> (T.pack $ replicate (childSpacing settings) ' ') <> "editedBy Text\n"
                                             <> (T.pack $ replicate (childSpacing settings) ' ') <> "editedOn UTCTime\n"
                                             
printTopLevel settings (PersistModelFileComment    c) = case keepComments settings of
                                                  True  -> _getComment c
                                                  False -> ""

printTopLevel settings (PersistModelFileWhiteSpace w) = case keepSpacing settings of
                                                  True -> _getWhiteSpace w
                                                  False -> ""

-- | Convert an 'EntityChild' to a piece of an Audit Model in 'Text'.
printEntityChild :: AuditGeneratorSettings -> EntityChild -> Text
printEntityChild settings (EntityChildEntityField  f) = "  " <> entityFieldName <> " "
                                                     <> r
                                                     <> entityFieldRest
                                                     <> "\n"
  where
    entityFieldName = _getEntityFieldName f
    entityFieldRest = _getEntityFieldRest f 
    eft = _getEntityFieldType f

    t   = _getEntityFieldTypeText eft
    
    eftShow = case _isEntityFieldTypeList eft of
      False -> t
      True  -> "[" <> t <> "]"
    
    r = case stringEndsInId $ T.unpack eftShow of
      False -> eftShow
      True  -> (printForeignKey (foreignKeyType settings) eftShow)

printEntityChild _        (EntityChildEntityUnique _) = ""
printEntityChild _        (EntityChildEntityDerive d) = "  " <> "deriving" <> " " <> _getEntityDeriveType d <> "\n"
printEntityChild settings (EntityChildComment      c) = case keepComments settings of 
                                                          True  -> _getComment c
                                                          False -> ""

printEntityChild settings (EntityChildWhiteSpace   w) = case keepSpacing settings of
                                                          True -> _getWhiteSpace w
                                                          False -> ""



-- | Convert a list of 'TopLevel' to a to a list of 'ToAudit' in 'Text'.
generateToAuditInstances ::  AuditGeneratorSettings -> PersistModelFile -> Text
generateToAuditInstances settings = T.concat . (map $ printToAuditInstance settings)

-- | Convert 'TopLevel' to an instance of 'ToAudit' in 'Text'.
printToAuditInstance :: AuditGeneratorSettings -> PersistModelFilePiece -> Text
printToAuditInstance settings (PersistModelFileEntity e) =  "instance ToAudit " <> entityName <> " where\n"
                                    <> "  type AuditResult " <> entityName <> " = " <> auditEntityName <> "\n"
                                    <> "  toAudit v k auditAction editedBy editedOn = " <> auditEntityName <> "\n"
                                    <> (T.concat $ map (printModelAccessor settings entityName) entityChildren)
                                    <> "    (" <> (printIfForeignKeyAlternate (foreignKeyType settings) "Id") <> "k) auditAction editedBy editedOn\n\n"
  where
    entityName = _getEntityName e
    auditEntityName = entityName <> (auditTag settings)
    entityChildren = _getEntityChildren e

printToAuditInstance _ _ = ""

-- encodeUtf8

-- | Convert 'EntityChild' to a Model accessor.
printModelAccessor :: AuditGeneratorSettings -> Text -> EntityChild -> Text
printModelAccessor settings entityName (EntityChildEntityField f) = "    ("
                                                        <> (printIfForeignKeyAlternate (foreignKeyType settings) (_getEntityFieldTypeText $ _getEntityFieldType f))
                                                        <> (T.pack . firstLetterToLowerCase . T.unpack $ entityName) 
                                                        <> (T.pack . firstLetterToUpperCase . T.unpack $ _getEntityFieldName f)
                                                        <> " v)\n"
printModelAccessor _ _ _ = ""


-- | Select the correct function for handling foreign keys.
printIfForeignKeyAlternate :: ForeignKeyType -> Text -> Text
printIfForeignKeyAlternate MongoKeyInSQL entityName = 
  case stringEndsInId $ T.unpack entityName of
    False -> ""
    True  -> "mongoKeyToByteString $ "

printIfForeignKeyAlternate SQLKeyInMongo entityName = 
  case stringEndsInId $ T.unpack entityName of
    False -> ""
    True  -> "fromSqlKey $ "

printIfForeignKeyAlternate _ _ = ""


-- | Return true if the last two characters are "Id".
stringEndsInId :: String -> Bool
stringEndsInId s = case length s >= 2 of
    False -> False
    True -> hasId $ reverse s
  where
    hasId ('d':'I':_) = True
    hasId _ = False

-- Id list

-- list or maybe <$>, otherwise $ 


-- | Convert the first letter of a 'String' to the corresponding uppercase letter.
firstLetterToUpperCase :: String -> String
firstLetterToUpperCase (h:r) = toUpper h : r
firstLetterToUpperCase _     = []

-- | Convert the first letter of a 'String' to the corresponsing lowercase letter.
firstLetterToLowerCase :: String -> String
firstLetterToLowerCase (h:r) = toLower h : r
firstLetterToLowerCase _     = []


-}