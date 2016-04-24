{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Attoparsec.Text

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.IO (putStr, readFile)

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
  modelsInputFile  :: Maybe FilePath
, modelsOutputFile :: Maybe FilePath
} deriving (Generic, Show, Eq)

instance HasArguments CmdOptions

main :: IO ()
main = withCliModified mods $ \ (f :: CmdOptions) -> do
  
  m <- Data.Text.IO.readFile "modelsMongo"
  
  let x = parseOnly parseEntities m

  case x of 
    Left _ -> return ()
    Right r -> Data.Text.IO.putStr $ generateAuditModels defaultSettings r
  
  where
    mods =
      AddShortOption "modelsInputFile"  'i' :
      AddShortOption "modelsOutputFile" 'o' :
      []