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

-- delete based on id
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
      _ <- insert (toAudit val key Database.Persist.Audit.Types.Delete userName now)
      delete key
      return ()


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
      _ <- insert (toAudit val key Database.Persist.Audit.Types.Update userName now)
      return ()


-- PersistQuery


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
{-
                       ( MonadIO m
                       , PersistStore backend
                       , PersistQueryWrite backend
                       , backend ~ BaseBackend backend
                       , backend ~ PersistEntityBackend val
                       , backend ~ PersistEntityBackend (AuditResult val)
                       , PersistEntity val
                       , PersistEntity (AuditResult val)
                       , ToAudit val) =>
-} 
                       [Filter val] ->  
                       Text -> 
                       ReaderT backend m ()
deleteWhereAndAudit filters userName = do
  toBeDeleted <- selectList filters []
  now <- liftIO $ getCurrentTime
  forM_ toBeDeleted $ \e -> do
    _ <- insert (toAudit (entityVal e) (entityKey e) Database.Persist.Audit.Types.Delete userName now)
    return ()

  deleteWhere filters
  return ()


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
  forM_ toBeUpdated $ \e -> do
    _ <- insert (toAudit (entityVal e) (entityKey e) Database.Persist.Audit.Types.Update userName now)
    return ()

  updateWhere filters updates
  return ()
