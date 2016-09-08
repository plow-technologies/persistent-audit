{-|
Module      : Database.Persist.Audit.Queries
Description : Persistent queries extended to audit changes
Copyright   : (c) James M.C. Haver II
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : Beta

This module provides queries that extend the original Persistent queries to
insert the changes they make into the corresponding audit columns.
-}


{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}

module Database.Persist.Audit.Queries where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Data.Text (Text)
import Data.Time

import Database.Persist
import Database.Persist.Audit.Class
import Database.Persist.Audit.Types

-- PersistStore

-- | Run Persistent 'insert' and insert data into the corresponding audit table.
insertAndAudit :: ( MonadIO m
#if MIN_VERSION_persistent(2,5,0)
                  , backend ~ BaseBackend backend
#endif
                  , backend ~ PersistEntityBackend val
                  , backend ~ PersistEntityBackend (AuditResult val)
                  , PersistEntity val
                  , PersistEntity (AuditResult val)
                  , PersistStore backend
                  , ToAudit val) =>
                  val ->
                  Text ->
                  ReaderT backend m (Key val)
insertAndAudit val userName = do
  key <- insert val
  now <- liftIO $ getCurrentTime
  _ <- insert (toAudit val key Database.Persist.Audit.Types.Create userName now)
  return key

-- | Run Persistent 'insertUnique' and insert data into the corresponding audit table.
insertUniqueAndAudit :: ( MonadIO m
#if MIN_VERSION_persistent(2,5,0)
                        , backend ~ BaseBackend backend
#endif
                        , backend ~ PersistEntityBackend val
                        , backend ~ PersistEntityBackend (AuditResult val)
                        , PersistEntity val
                        , PersistEntity (AuditResult val)
                        , PersistStore backend
                        , PersistUnique backend
                        , ToAudit val) =>
                        val ->
                        Text ->
                        ReaderT backend m (Maybe (Key val))
insertUniqueAndAudit val userName = do
  mKey <- insertUnique val
  now <- liftIO $ getCurrentTime
  case mKey of
    Nothing -> return ()
    Just key -> void $ insert (toAudit val key Database.Persist.Audit.Types.Create userName now)
  return mKey

-- | Run Persistent 'delete' and insert data into the corresponding audit table.
deleteAndAudit :: ( MonadIO m
#if MIN_VERSION_persistent(2,5,0)
                  , backend ~ BaseBackend backend
#endif
                  , backend ~ PersistEntityBackend val
                  , backend ~ PersistEntityBackend (AuditResult val)
                  , PersistEntity val
                  , PersistEntity (AuditResult val)
                  , PersistStore backend
                  , ToAudit val) =>
                  Key val ->
                  Text ->
                  ReaderT backend m ()
deleteAndAudit key userName = do
  mVal <- get key
  case mVal of
    Nothing -> return ()
    Just val -> do
      now <- liftIO $ getCurrentTime
      void $ insert (toAudit val key Database.Persist.Audit.Types.Delete userName now)
      delete key

-- | Run Persistent 'update' and insert data into the corresponding audit table.
updateAndAudit :: ( MonadIO m
#if MIN_VERSION_persistent(2,5,0)
                  , backend ~ BaseBackend backend
#endif
                  , backend ~ PersistEntityBackend val
                  , backend ~ PersistEntityBackend (AuditResult val)
                  , PersistEntity val
                  , PersistEntity (AuditResult val)
                  , PersistStore backend
                  , ToAudit val) =>
                   Key val ->
                   [Update val] ->
                   Text ->
                   ReaderT backend m ()
updateAndAudit key updateVals userName = do
  update key updateVals
  mVal <- get key
  case mVal of
    Nothing -> return ()
    Just val -> do
      now <- liftIO $ getCurrentTime
      void $ insert (toAudit val key Database.Persist.Audit.Types.Update userName now)


-- PersistQuery

-- | Run Persistent 'deleteWhere' and insert data into the corresponding audit table.
deleteWhereAndAudit :: ( MonadIO m
#if MIN_VERSION_persistent(2,5,0)
                       , backend ~ BaseBackend backend
                       , PersistQueryWrite backend
#else
                       , PersistQuery backend
#endif
                       , backend ~ PersistEntityBackend val
                       , backend ~ PersistEntityBackend (AuditResult val)
                       , PersistEntity val
                       , PersistEntity (AuditResult val)
                       , PersistStore backend
                       , ToAudit val) =>
                       [Filter val] ->
                       Text ->
                       ReaderT backend m ()
deleteWhereAndAudit filters userName = do
  toBeDeleted <- selectList filters []
  now <- liftIO $ getCurrentTime
  forM_ toBeDeleted $ \e -> void $  insert (toAudit (entityVal e) (entityKey e) Database.Persist.Audit.Types.Delete userName now)
  deleteWhere filters

-- | Run Persistent 'deleteBy' and insert data into the corresponding audit table.
deleteByAndAudit :: ( MonadIO m
#if MIN_VERSION_persistent(2,5,0)
                       , backend ~ BaseBackend backend
                       , PersistQueryWrite backend
#else
                       , PersistQuery backend
#endif
                       , backend ~ PersistEntityBackend val
                       , backend ~ PersistEntityBackend (AuditResult val)
                       , PersistEntity val
                       , PersistEntity (AuditResult val)
                       , PersistStore backend
                       , PersistUnique backend
                       , ToAudit val)
                       => Unique val
                       -> Text
                       -> ReaderT backend m ()
deleteByAndAudit uniqueKey userName = do
  mEntity <- getBy uniqueKey
  deleteBy uniqueKey
  case mEntity of
    Nothing -> return ()
    Just entity -> do
      now <- liftIO $ getCurrentTime
      void $ insert (toAudit (entityVal entity) (entityKey entity) Database.Persist.Audit.Types.Delete userName now)

-- | Run Persistent 'updateWhere' and insert data into the corresponding audit table.
updateWhereAndAudit :: ( MonadIO m
#if MIN_VERSION_persistent(2,5,0)
                       , backend ~ BaseBackend backend
                       , PersistQueryWrite backend
#else
                       , PersistQuery backend
#endif
                       , backend ~ PersistEntityBackend val
                       , backend ~ PersistEntityBackend (AuditResult val)
                       , PersistEntity val
                       , PersistEntity (AuditResult val)
                       , PersistStore backend
                       , ToAudit val) =>
                       [Filter val] ->
                       [Update val] ->
                       Text ->
                       ReaderT backend m ()
updateWhereAndAudit filters updates userName = do
  toBeUpdated <- selectList filters []
  now <- liftIO $ getCurrentTime
  forM_ toBeUpdated $ \e -> void $ insert (toAudit (entityVal e) (entityKey e) Database.Persist.Audit.Types.Update userName now)
  updateWhere filters updates

-- | Run Persistent 'repsert' and insert data into the corresponding audit table.
repsertAndAudit :: ( MonadIO m
#if MIN_VERSION_persistent(2,5,0)
                  , backend ~ BaseBackend backend
#endif
                  , backend ~ PersistEntityBackend val
                  , backend ~ PersistEntityBackend (AuditResult val)
                  , PersistEntity val
                  , PersistEntity (AuditResult val)
                  , PersistStore backend
                  , ToAudit val)
                  => Key val
                  -> val
                  -> Text
                  -> ReaderT backend m (Key val)
repsertAndAudit key val userName = do
  mVal <- get key
  repsert key val
  auditType <- case mVal of
    Nothing -> return Database.Persist.Audit.Types.Create
    Just _  -> return Database.Persist.Audit.Types.Update

  now <- liftIO $ getCurrentTime
  _ <- insert (toAudit val key auditType userName now)
  return key

-- | Run Persistent 'replace' and insert data into the corresponding audit table.
replaceAndAudit :: ( MonadIO m
#if MIN_VERSION_persistent(2,5,0)
                  , backend ~ BaseBackend backend
#endif
                  , backend ~ PersistEntityBackend val
                  , backend ~ PersistEntityBackend (AuditResult val)
                  , PersistEntity val
                  , PersistEntity (AuditResult val)
                  , PersistStore backend
                  , ToAudit val)
                  => Key val
                  -> val
                  -> Text
                  -> ReaderT backend m ()
replaceAndAudit key val userName = do
  mVal <- get key
  replace key val
  case mVal of
    Nothing -> return ()
    Just _  -> do
      now <- liftIO $ getCurrentTime
      void $ insert (toAudit val key Database.Persist.Audit.Types.Update userName now)
