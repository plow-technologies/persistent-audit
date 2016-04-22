module Database.Persist.Audit.Types where

import           Data.Text (Text)


data TopLevel    = TopLevelEntity     Entity     |
                   TopLevelComment    Comment    |
                   TopLevelWhiteSpace WhiteSpace  
  deriving (Eq,Show,Read)


data EntityChild = EntityChildEntityField  EntityField  |
                   EntityChildEntityUnique EntityUnique |
                   EntityChildEntityDerive EntityDerive |
                   EntityChildComment      Comment      |
                   EntityChildWhiteSpace   WhiteSpace
  deriving (Eq,Show,Read)

-- Entity has multiple FieldNames and FieldTypes
data Entity = Entity {
  _getEntityName     :: Text
, _getEntityChildren :: [EntityChild]
} deriving (Eq,Show,Read)

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


{-
data EntityDerive = EntityDerive {
  _getEntityDeriveType :: EntityDeriveType
} deriving (Eq,Show,Read)

data EntityDeriveType = EntityDeriveType {
  _getEntityDeriveTypeText :: Text
} deriving (Eq,Show,Read)

data EntityUniqueName = EntityUniqueName {
  _getEntityUniqueNameText :: Text
} deriving (Eq,Show,Read)

data EntityUniqueEntityFieldName = EntityUniqueEntityFieldName { 
  _getEntityUniqueEntityFieldNameText :: Text 
} deriving (Eq,Show,Read)

data EntityName = EntityName {
  _getEntityNameText :: Text 
} deriving (Eq,Show,Read)

data EntityFieldName = EntityFieldName { 
  _getEntityFieldNameText :: Text 
} deriving (Eq,Show,Read)
-}



data WhiteSpace = WhiteSpace {
  _getWhiteSpace :: Text
} deriving (Eq,Show,Read)

data Comment = Comment {
  _getComment :: Text 
} deriving (Eq,Show,Read)