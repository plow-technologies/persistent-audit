{-|
Module      : Database.Persist.Audit.Class
Description : Generalize conversion of a type to an audit type
Copyright   : (c) James M.C. Haver II
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : Beta

Persistent Model types that have a corresponding audit type as produced by
'Database.Persist.Audit.Generator.generateAuditModels' need to implement
'ToAudit' in order to use the queries from 'Database.Persist.Audit.Queries'.
Given the following two Persistent models:

@
Person
  name String
  age Int Maybe

PersonAudit
  name String
  age Int Maybe
  originalId PersonId noreference
  auditAction AuditAction
  editedBy Text
  editedOn UTCTime
@

The 'ToAudit' instance should look like this:

@
instance ToAudit Person where
 type AuditResult Person = PersonAudit
 toAudit v k auditAction editedBy editedOn = PersonAudit (personName v)
                                                         (personAge v)
                                                         k auditAction editedBy editedOn
@
-}


{-# LANGUAGE TypeFamilies #-}

module Database.Persist.Audit.Class where

import           Data.Text              (Text)
import           Data.Time              (UTCTime)
import           Database.Persist.Types
import           Database.Persist.Audit.Types

-- | 'toAudit' is a general way to convert a model into an audit model.
class ToAudit a where
  type AuditResult a :: *
  toAudit :: a -> Key a -> AuditAction -> Text -> UTCTime -> AuditResult a
