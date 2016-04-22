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


data CmdOptions = CmdOptions {
  modelsInputFile  :: Maybe FilePath
, modelsOutputFile :: Maybe FilePath
} deriving (Generic, Show, Eq)

instance HasArguments CmdOptions

main :: IO ()
main = withCliModified mods $ \ (f :: CmdOptions) -> do
  -- print f
  
  m <- Data.Text.IO.readFile "modelsMongo"
  -- print $ parseOnly parseEntities "Person json\n name Text\n birth UTCTime\n UniqueName name\n deriving Eq\nGuy"

  -- print $ parseOnly parseEntities "Person json\n name Text"

  --print m
  let x = parseOnly parseEntities m

  case x of 
    Left _ -> return ()
    Right r -> Data.Text.IO.putStr $ generateAuditModels r
  -- print m
  {-
  print $ parseOnly variableP "abc"
  print $ parseOnly variableP "abc1"
  print $ parseOnly variableP "ABC"
  print $ parseOnly variableP "dfs"
  print $ parseOnly variableP "12dfs"
  print $ parseOnly lowerCaseOrUnderline "_"
  print $ parseOnly lowerCaseOrUnderline "a"
  print $ parseOnly lowerCaseOrUnderline "A"
  print $ parseOnly lowerCaseOrUnderline "1"

  print $ parseOnly haskellFunctionName "_get  "
  print $ parseOnly haskellFunctionName "_get1  "
  print $ parseOnly haskellFunctionName "get  "
  print $ parseOnly haskellFunctionName "get_  "
  print $ parseOnly haskellFunctionName "get_1  "
  print $ parseOnly haskellFunctionName "Get"
  print $ parseOnly haskellFunctionName "1et"

  print $ parseOnly parseEntityName "Person"
  print $ parseOnly parseEntityName "PersonHome"
  print $ parseOnly parseEntityName "per"
  print $ parseOnly parseEntityName "_Per"

  print $ parseOnly parseEntity "Person"
  print $ parseOnly parseEntity "Person\n name Text\n birth UTCTime"
  print $ parseOnly parseEntity "Person\n name Text\n birth UTCTime\n UniqueName name\n deriving Eq"
  -}
  where
    mods =
      AddShortOption "modelsInputFile"  'i' :
      AddShortOption "modelsOutputFile" 'o' :
      
      []
  --args <- getArgs
  -- f <- Data.Text.IO.readFile "modelsMongo"
  --print args
  -- print $ parseOnly parseEntityName "ABC"
  -- print $ parseOnly parseEntityName "ABC "
  -- print $ parseOnly parseEntityName "ABC   "
  -- print $ parseOnly parseEntity "Person \n thing Int "
  -- print $ parseOnly parseEntity "Person\n thing  Int"
  
  -- print $ parseOnly parseEntityField " thing Int "  
  -- print $ parseOnly parseEntity "--Stuff  \n  \nPerson\n thing  Int\n that Text\n address Text default = \"\"\n deriving Eq"
  
  {-
  print $ parseOnly parseEntity "Person\n names [Int]"

  print $ parseOnly parseEntity "Dashboard\n  owner UserId Maybe\n  gId GroupId Maybe\n  group Group Maybe\n  gPermissions UserPermissionsList Maybe\n  oPermissions UserPermissionsList Maybe\n  name Text Maybe\n  notes Text Maybe\n  default Bool Maybe\n  header HeaderWidget Maybe\n  panels [MenuPanel] Maybe"
  

  print $ parseOnly parseEntity "Group\n  owner UserId    -- who made this group\n  group GroupId  -- owner group\n  name Text\n  description Textarea\n  UniqueGroup name\n  deriving Typeable\n  deriving Read\n  deriving Show\n  deriving Eq"
  
  print $ parseOnly parseEntities f 
  -}
  --print $ parseOnly parseEntityUnique " Unique group"

  -- print $ parseOnly parseEntityDerive " deriving Eq "
  -- print $ parseOnly parseEntity "Person\n thing  Int\n that Text\n address Text default = \"\"\n Unique group"
  

  {-
  print $ parseOnly parseEntityFieldName " person "
  print $ parseOnly parseEntityFieldType "[Int]"

  print $ parseOnly parseMultiLineComment  "{-   -}"

  print $ parseOnly parseEntity "Person\n thing"
  -}