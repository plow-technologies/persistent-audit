{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}

module Database.Persist.Audit.Queries where


import Control.Monad.IO.Class
import Control.Monad.Trans.Reader  

import Data.Text (Text)
import Data.Time

import Database.Persist
import Database.Persist.Audit.Class
import Database.Persist.Class



insertWithAudit :: ( MonadIO m
                   , PersistStore backend
                   , backend ~ BaseBackend backend
                   , backend ~ PersistEntityBackend val
                   , backend ~ PersistEntityBackend (AuditResult val)
                   , PersistEntity val
                   , PersistEntity (AuditResult val)
                   , ToAudit val) => val -> Bool -> Text -> ReaderT backend m (Key val) 
insertWithAudit entity delete userName = do
  key <- insert entity
  now <- liftIO $ getCurrentTime
  _ <- insert (toAudit entity key delete userName now)
  return key

-- delete
-- update