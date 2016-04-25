{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.Audit.Generator where

import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T

import           Database.Persist.Audit.Types



-- import           Database.Persist.Types (PersistValue)



data AuditGeneratorSettings = AuditGeneratorSettings {
  childSpacing :: Int
, auditTag     :: Text
, keepEntityDerive :: Bool
, keepComments :: Bool
, keepSpacing  :: Bool
} deriving (Eq,Show,Read)

defaultSettings :: AuditGeneratorSettings
defaultSettings =  AuditGeneratorSettings 2 "History" True False False

-- replace UserId with PersistValue
-- replicate (childSpacing settings) ' '

generateAuditModels :: AuditGeneratorSettings -> [TopLevel] -> Text
generateAuditModels settings = T.concat . (map $ (flip T.append "\n") . (printTopLevel settings))

printTopLevel :: AuditGeneratorSettings -> TopLevel -> Text
printTopLevel settings (TopLevelEntity     e) = (_getEntityName e <> auditTag settings <> "\n")
                                             <> (T.concat $ map (printEntityChild settings) $ _getEntityChildren e)
                                             <> (T.pack $ replicate (childSpacing settings) ' ') <> "originalId " <> (_getEntityName e)<> "Id noreference\n"
                                             <> (T.pack $ replicate (childSpacing settings) ' ') <> "deleted Bool\n"
                                             <> (T.pack $ replicate (childSpacing settings) ' ') <> "editedBy Text\n"
                                             <> (T.pack $ replicate (childSpacing settings) ' ') <> "editedOn UTCTime\n"
                                             
printTopLevel settings (TopLevelComment    c) = case keepComments settings of
                                                  True  -> _getComment c
                                                  False -> ""

printTopLevel settings (TopLevelWhiteSpace w) = case keepComments settings of
                                                  True -> _getWhiteSpace w
                                                  False -> ""


printEntityChild :: AuditGeneratorSettings -> EntityChild -> Text
printEntityChild settings (EntityChildEntityField  f) = "  " <> _getEntityFieldName f <> " "
                                                    <> r
                                                    <> _getEntityFieldRest f
                                                    <> "\n"
  where
    eft = _getEntityFieldType f
    t   = _getEntityFieldTypeText eft
    eftShow = case _isEntityFieldTypeList eft of
      False -> t
      True  -> "[" <> t <> "]"
    r = case stringEndsInId $ T.unpack eftShow of
      False -> eftShow
      True  -> "PersistValue"

printEntityChild settings (EntityChildEntityUnique u) = ""
                                                    {- "  " <> _getEntityUniqueName u <> " " 
                                                    <> _getEntityUniqueEntityFieldName u 
                                                    <> _getEntityUniqueRest u 
                                                    <> "\n"
                                                    -}
printEntityChild settings (EntityChildEntityDerive d) = "  " <> "deriving" <> " " <> _getEntityDeriveType d <> "\n"
printEntityChild settings (EntityChildComment      c) = case keepComments settings of 
                                                          True  -> _getComment c
                                                          False -> ""
printEntityChild settings (EntityChildWhiteSpace   w) = ""


-- check if the last two characters of a string are "Id"
stringEndsInId :: String -> Bool
stringEndsInId s = case length s >= 2 of
    False -> False
    True -> hasId $ reverse s
  where
    hasId ('d':'I':_) = True
    hasId _ = False
