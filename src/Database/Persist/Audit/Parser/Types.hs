
{-# LANGUAGE OverloadedStrings   #-}

module Database.Persist.Audit.Parser.Types where

import Data.Text (Text)

data MigrationOnlyAndSafeToRemoveOption = MigrationOnly | SafeToRemove deriving (Eq,Read,Show)

data EntityFieldLastItem = FieldDefault Text
                         | FieldSqlRow  Text 
                         | FieldSqlType Text
                         | FieldMaxLen  Int
  deriving (Read,Show)

instance Eq EntityFieldLastItem where
  (FieldDefault  _) == (FieldDefault  _) = True
  (FieldSqlRow   _) == (FieldSqlRow   _) = True
  (FieldSqlType  _) == (FieldSqlType  _) = True
  (FieldMaxLen   _) == (FieldMaxLen   _) = True
  _ == _ = False
