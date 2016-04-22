module Database.Persist.Audit.Types where

import           Data.Text (Text)

-- Entity has multiple FieldNames and FieldTypes
data Entity = Entity {
  _getEntityName    :: EntityName
, _getEntityFields  :: [EntityField]
, _getEntityUniques :: [EntityUnique]
, _getEntityDerives :: [EntityDerive]
} deriving (Eq,Show,Read)

data EntityChild = EntityChildEntityField  EntityField  |
                   EntityChildEntityUnique EntityUnique |
                   EntityChildEntityDerive EntityDerive |
                   EntityChildSpace        ()

catEntityFields :: [EntityChild] -> [EntityField]
catEntityFields ls = [x | EntityChildEntityField x <- ls]

catEntityUniques :: [EntityChild] -> [EntityUnique]
catEntityUniques ls = [x | EntityChildEntityUnique x <- ls]

catEntityChildEntityDerives :: [EntityChild] -> [EntityDerive]
catEntityChildEntityDerives ls = [x | EntityChildEntityDerive x <- ls]


data EntityField = EntityField {
  _getEntityFieldName :: EntityFieldName
, _getEntityFieldType :: EntityFieldType
, _getEntityFieldRest :: Text  
} deriving (Eq,Show,Read)

data EntityUnique = EntityUnique {
  _getEntityUniqueName            ::  EntityUniqueName
, _getEntityUniqueEntityFieldName ::  EntityUniqueEntityFieldName
, _getEntityUniqueRest            ::  Text
} deriving (Eq,Show,Read)

data EntityDerive = EntityDerive {
  _getEntityDeriveType :: EntityDeriveType
} deriving (Eq,Show,Read)

data EntityDeriveType = EntityDeriveType Text deriving (Eq,Show,Read)

data EntityUniqueName = EntityUniqueName Text deriving (Eq,Show,Read)

data EntityUniqueEntityFieldName = EntityUniqueEntityFieldName Text deriving (Eq,Show,Read)

data EntityName = EntityName Text deriving (Eq,Show,Read)

data EntityFieldName = EntityFieldName Text deriving (Eq,Show,Read)

data EntityFieldType = EntityFieldType Text Bool deriving (Eq,Show,Read)
