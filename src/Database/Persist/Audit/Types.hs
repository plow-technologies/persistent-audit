{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Persist.Audit.Types where

import           Control.Applicative (empty)
import           Control.Monad       (mzero)

import           Data.Aeson
import           Data.Hashable
import qualified Data.HashMap.Lazy as HML
import           Data.Text           (Text)

import           Database.Persist.TH

import           GHC.Generics

-- | A collection of data types with which you can recontruct a Persist Model file
-- | or create an altered version.
type PersistModelFile = [PersistModelFilePiece]

-- | Top level pieces of a Persist Model file.
data PersistModelFilePiece = PersistModelFileEntity     Entity     |
                             PersistModelFileComment    Comment    |
                             PersistModelFileWhiteSpace WhiteSpace
  deriving (Eq,Show,Read)

-- | A single Persist Model Entity.
data Entity = Entity {
  _getEntityName      :: Text
, _isEntityDeriveJson :: Bool          -- | Person json
, _getEntitySqlTable  :: Maybe Text    -- | Person sql=peoples
, _getEntityChildren  :: [EntityChild]
} deriving (Eq,Show,Read)


-- | All of the child elements of a Persist Model Entity.
-- | They are all indented in the Model File.
data EntityChild = EntityChildEntityField   EntityField   |
                   EntityChildEntityUnique  EntityUnique  |
                   EntityChildEntityDerive  EntityDerive  |
                   EntityChildEntityPrimary EntityPrimary |
                   EntityChildEntityForeign EntityForeign |
                   EntityChildComment       Comment       |
                   EntityChildWhiteSpace    WhiteSpace
  deriving (Eq,Show,Read)

-- | A data row from an Entity.
data EntityField = EntityField {
  _getEntityFieldName         :: Text
, _getEntityFieldType         :: EntityFieldType
, _isEntityFieldMigrationOnly :: Bool  -- | MigrationOnly
, _isEntityFieldSafeToRemove  :: Bool  -- | SafeToRemove
, _getEntityFieldDefault      :: Maybe Text -- | default=Nothing, default=now(), default=CURRENT_DATE
, _getEntityFieldSqlRow       :: Maybe Text -- | sql=my_id_name
, _getEntityFieldSqlType      :: Maybe Text -- | sqltype=varchar(255)
, _getEntityFieldMaxLen       :: Maybe Int
} deriving (Eq,Show,Read)


-- | Table rows can be strict or lazy
data Strictness
  -- | Persist Model types are strict without any notation
  = Strict
  -- | "!" can be used to reemphasize that a type is strict
  | ExplicitStrict
  -- | "~" means that a type is Lazy
  | Lazy
  deriving (Eq,Show,Read)
-- | An entity data row's type. If '_isEntityFieldTypeList' is 'True' than this type is a list.
data EntityFieldType = EntityFieldType {
  _getEntityFieldTypeText   :: Text
, _getEntityFieldStrictness :: Strictness
, _isEntityFieldTypeList    :: Bool
, _isEntityFieldTypeMaybe   :: Bool
} deriving (Eq,Show,Read)


-- | A unique idenfitier for an Entity.
data EntityUnique = EntityUnique {
  _getEntityUniqueName            ::  Text
, _getEntityUniqueEntityFieldName ::  [Text]
} deriving (Eq,Show,Read)


-- | 'deriving Eq', 'deriving Show', etc.
data EntityDerive = EntityDerive {
  _getEntityDeriveTypes :: [Text]
} deriving (Eq,Show,Read)

-- | 'Primary name'
data EntityPrimary = EntityPrimary {
  _getEntityPrimeType :: [Text]
} deriving (Eq,Show,Read)

-- | 'Foreign Tree fkparent parent'
data EntityForeign = EntityForeign {
  _getEntityForeignTable :: Text
, _getEntityForeignTypes :: [Text]
} deriving (Eq,Show,Read)


-- | Any white spaces that the user might want to maintain when generating Audit Models.
data WhiteSpace = WhiteSpace {
  _getWhiteSpace :: Text
} deriving (Eq,Show,Read)

-- | Haskell style comments that start with "-- "
data Comment = Comment {
  _getComment :: Text
} deriving (Eq,Show,Read)


-- | Annotations for each Audit Model to keep track of why it was inserted.
data AuditAction = Create | Delete | Update
  deriving (Show, Read, Eq, Ord, Generic)

derivePersistField "AuditAction"

instance Hashable AuditAction
instance FromJSON AuditAction where
  parseJSON (Object o) = getAuditAction
    where
      getAuditAction
       | HML.member "Create" o = pure Database.Persist.Audit.Types.Create
       | HML.member "Delete" o = pure Database.Persist.Audit.Types.Delete
       | HML.member "Update" o = pure Database.Persist.Audit.Types.Update
       | True                  = empty

  parseJSON _          = mzero

instance ToJSON AuditAction where
  toJSON (Database.Persist.Audit.Types.Create) = object ["Create" .= ([] :: [Int])]
  toJSON (Database.Persist.Audit.Types.Delete) = object ["Delete" .= ([] :: [Int])]
  toJSON (Database.Persist.Audit.Types.Update) = object ["Update" .= ([] :: [Int])]
