{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.Audit.Generator where

import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T

import           Database.Persist.Audit.Types



-- import           Database.Persist.Types (PersistValue)


-- isSuffixOf

{-
stringEndsInId :: String -> (String,Bool)
stringEndsInId s = case length s >= 2 of
    False -> (s,False)
    True -> hasId rs
  where
    rs = reverse s
    hasId ('d':'I':rest) = (reverse rest,True)
    hasId rest = (reverse rest,False)
-}
stringEndsInId :: String -> Bool
stringEndsInId s = case length s >= 2 of
    False -> False
    True -> hasId $ reverse s
  where
    hasId ('d':'I':_) = True
    hasId _ = False



data AuditGeneratorSettings = AuditGeneratorSettings {
  childSpacing :: Int
, auditTag     :: Text
} deriving (Eq,Show,Read)

defaultSettings :: AuditGeneratorSettings
defaultSettings =  AuditGeneratorSettings 2 "History"

-- replace UserId with PersistValue
-- replicate (childSpacing settings) ' '

generateAuditModels :: AuditGeneratorSettings -> [TopLevel] -> Text
generateAuditModels settings = T.concat . (map $ (flip T.append "\n") . (printTopLevel settings))

printTopLevel :: AuditGeneratorSettings -> TopLevel -> Text
printTopLevel settings (TopLevelEntity     e) = (_getEntityName e <> auditTag settings <> "\n")
                                             <> (T.concat $ map (printEntityChild settings) $ _getEntityChildren e)
                                             <> (T.pack $ replicate (childSpacing settings) ' ') <> "originalId PersistValue\n"
                                             <> (T.pack $ replicate (childSpacing settings) ' ') <> "deleted Bool\n"
                                             <> (T.pack $ replicate (childSpacing settings) ' ') <> "editedBy Text\n"
                                             <> (T.pack $ replicate (childSpacing settings) ' ') <> "editedOn UTCTime\n"
                                             
printTopLevel settings (TopLevelComment    c) = _getComment c
printTopLevel settings (TopLevelWhiteSpace w) = _getWhiteSpace w


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
printEntityChild settings (EntityChildComment      c) = _getComment c <> "\n"
printEntityChild settings (EntityChildWhiteSpace   w) = ""
