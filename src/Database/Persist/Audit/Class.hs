{-# LANGUAGE TypeFamilies #-}
module Database.Persist.Audit.Class where

import           Data.Text              (Text)
import           Data.Time              (UTCTime)
import           Database.Persist.Types
import           Database.Persist.Audit.Types

class ToAudit a where
  type AuditResult a :: *
  toAudit :: a -> Key a -> AuditAction -> Text -> UTCTime -> AuditResult a


{-
instance ToAudit KioskServerConfig KioskServerConfigHistory where
  toAudit v k deleted editedBy editedOn = KioskServerConfigHistory (kioskServerConfigIp v)
                                                                   (kioskServerConfigPort v)
                                                                   (kioskServerConfigName v)
                                                                   k deleted editedBy editedOn 


updateWithAudit :: ToAudit a b => a -> Key a -> IO ()
updateWithAudit x c = do
  now <- getCurrentTime 
  let z = toAudit x c False "Me" now
  -- do db stuff now
  return ()
-}