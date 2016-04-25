{-# LANGUAGE TemplateHaskell #-}

module Database.Persist.Audit.Types where

import           Data.Text (Text)

import           Database.Persist.TH


data TopLevel    = TopLevelEntity     Entity     |
                   TopLevelComment    Comment    |
                   TopLevelWhiteSpace WhiteSpace  
  deriving (Eq,Show,Read)

-- Entity has multiple FieldNames and FieldTypes
data Entity = Entity {
  _getEntityName     :: Text
, _getEntityChildren :: [EntityChild]
} deriving (Eq,Show,Read)

-- entityrest

data EntityChild = EntityChildEntityField  EntityField  |
                   EntityChildEntityUnique EntityUnique |
                   EntityChildEntityDerive EntityDerive |
                   EntityChildComment      Comment      |
                   EntityChildWhiteSpace   WhiteSpace
  deriving (Eq,Show,Read)

data EntityField = EntityField {
  _getEntityFieldName :: Text
, _getEntityFieldType :: EntityFieldType
, _getEntityFieldRest :: Text  
} deriving (Eq,Show,Read)

data EntityUnique = EntityUnique {
  _getEntityUniqueName            ::  Text
, _getEntityUniqueEntityFieldName ::  Text
, _getEntityUniqueRest            ::  Text
} deriving (Eq,Show,Read)

data EntityFieldType = EntityFieldType {
  _getEntityFieldTypeText ::  Text 
, _isEntityFieldTypeList  ::  Bool
} deriving (Eq,Show,Read)

data EntityDerive = EntityDerive {
  _getEntityDeriveType :: Text
} deriving (Eq,Show,Read)


data WhiteSpace = WhiteSpace {
  _getWhiteSpace :: Text
} deriving (Eq,Show,Read)

data Comment = Comment {
  _getComment :: Text 
} deriving (Eq,Show,Read)


data AuditAction = Create | Delete | Update 
  deriving (Show, Read, Eq)

derivePersistField "AuditAction"
