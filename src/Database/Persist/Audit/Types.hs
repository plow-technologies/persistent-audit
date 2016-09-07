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
  
