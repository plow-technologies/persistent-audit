{-|
Module      : Database.Persist.Audit.MongoDB.Util
Description : Convert a MongoKey to and from ByteString
Copyright   : (c) James M.C. Haver II
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : Beta

Functions to convert a MongoKey to and from 'ByteString'. This is useful if your
models are stored in MongoDB and your model audits are stored in SQL.

@
User
  ident Text
  password Text Maybe
  UniqueUser ident

UserAudit
  ident Text
  password Text Maybe
  originalId ByteString
  auditAction AuditAction
  editedBy Text
  editedOn UTCTime

instance ToAudit User where
  type AuditResult User = UserAudit
  toAudit v k auditAction editedBy editedOn = UserAudit (userIdent v)
    (userPassword v)
    (mongoKeyToByteString k) auditAction editedBy editedOn
@
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.Audit.MongoDB.Util where

import           Data.ByteString          (ByteString)
import           Data.Text.Encoding       (decodeUtf8',encodeUtf8)

import           Database.MongoDB as DB
import           Database.Persist.MongoDB (keyToText,readMayMongoKey)
import           Database.Persist


-- | Used for creating instances of ToAudit when the original model database is Mongo
--   and the audit model database is SQL.
mongoKeyToByteString :: (ToBackendKey DB.MongoContext record) => Key record -> ByteString
mongoKeyToByteString = encodeUtf8 . keyToText . toBackendKey

-- | Used for when Mongo stored in SQL needs to be converted to a Key to query the original
--   MongoDB.
byteStringToMongoKey :: (ToBackendKey DB.MongoContext record) => ByteString -> Maybe (Key record)
byteStringToMongoKey bs = case decodeUtf8' bs of
                            Left _ -> Nothing
                            Right text -> fromBackendKey <$> readMayMongoKey text

-- | To handle SQL keys to and from MongoDB, use 'toSqlKey' and 'fromSqlKey' from Database.Persist.Sql
