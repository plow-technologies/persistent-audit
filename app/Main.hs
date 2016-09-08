{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Text.IO

import           Database.Persist.Parser
import           Database.Persist.Audit.Generator

import           WithCli

{-
principles
  no foreign keys between audit table and original table, they store the same data but no dependency
  database independence, original models and audit models can be stored in separate dbs

  makes a best attempt, no guarantees

  assumptions made:
    types ending in Id are foreign pointers
-}

-- data CrossDB = SQLtoMongoDB | MongoDBtoSQL deriving (Eq,Generic,Read,Show, HasArguments)

data CmdOptions = CmdOptions {
  model         :: FilePath -- | Input: Model file to parse
, audit         :: FilePath -- | Ouput: Audit Model file
, auditInstance :: Maybe FilePath -- | Optional Output: ToAudit Instances for models in model file to models in Audit Model File
, crossDB       :: Maybe String -- | Nothing if original models and audit models are in the same database type
                              --   'sqlToMongoDB'
                              --   'mongoDbToSql'
} deriving (Generic, Show, Eq, Read)

instance HasArguments CmdOptions

main :: IO ()
main = withCliModified mods $ \ (ops :: CmdOptions) -> do
  m <- Data.Text.IO.readFile $ model ops
  settings <- case crossDB ops of
      Nothing -> return defaultSettings
      Just c  -> case c of
        "mongoDbToSql" -> return $ defaultSettings {foreignKeyType = MongoKeyInSQL}
        "sqlToMongoDB" -> return $ defaultSettings {foreignKeyType = SQLKeyInMongo}
        _              -> return defaultSettings

  case parseModelsFile m of
    Left _ -> print ("Failed to parse the models file with parseEntities function." :: String)
    Right models -> do
      Data.Text.IO.writeFile (audit ops) (generateAuditModels settings models)
      case auditInstance ops of
        Nothing -> return ()
        Just auditInstanceFile -> do
          Data.Text.IO.writeFile auditInstanceFile (generateToAuditInstances settings models)
          return ()

  where
    mods =
      AddShortOption "model" 'm' :
      AddShortOption "audit" 'a' :
      AddShortOption "auditInstance" 'i' :
      AddShortOption "crossDB" 'c' :
      []
