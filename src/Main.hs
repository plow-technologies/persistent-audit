{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.IO

import           Database.Persist.Audit.Generator
import           Database.Persist.Audit.Parser

import           WithCli

{-
principles
  no foreign keys between audit table and original table, they store the same data but no dependency
  database independence, original models and audit models can be stored in separate dbs

  makes a best attempt, no guarantees

  assumptions made: 
    types ending in Id are foreign pointers
-}

data CmdOptions = CmdOptions {
  model         :: FilePath
, audit         :: FilePath
, auditInstance :: Maybe FilePath
} deriving (Generic, Show, Eq)

instance HasArguments CmdOptions

main :: IO ()
main = withCliModified mods $ \ (ops :: CmdOptions) -> do
  
  m <- Data.Text.IO.readFile $ model ops

  case parseModelsFile m of 
    Left _ -> print $ "Failed to parse the models file with parseEntities function."
    Right models -> do
      Data.Text.IO.writeFile (audit ops) (generateAuditModels defaultSettings models)
      case auditInstance ops of
        Nothing -> return ()
        Just auditInstanceFile -> do
          Data.Text.IO.writeFile auditInstanceFile (generateToAuditInstances defaultSettings models)
          return ()
  

  where
    mods =
      AddShortOption "model" 'm' :
      AddShortOption "audit" 'a' :
      AddShortOption "auditInstance" 'i' :
      []