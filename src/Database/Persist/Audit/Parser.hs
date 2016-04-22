{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Persist.Audit.Parser where

import           Control.Applicative 

import           Data.Attoparsec.ByteString.Char8 (isSpace)
import           Data.Attoparsec.Text

import           Data.Text (Text)
import qualified Data.Text as T

import           Database.Persist.Audit.Types

import           Prelude hiding (takeWhile)


-- variableP :: Parser String
-- variableP = (++) <$> many1 letter <*> many (digit <|> letter) <* endOfInput
  
-- helper functions

maybeOption :: Parser a -> Parser (Maybe a)
maybeOption p = option Nothing (Just <$> p)

lowerCase :: Parser Char
lowerCase = satisfy (\char -> char >= 'a' && char <= 'z')

upperCase :: Parser Char
upperCase = satisfy (\char -> char >= 'A' && char <= 'Z')

underline :: Parser Char 
underline = satisfy (== '_')

someSpace :: Parser ()
someSpace = skipMany1 space


-- get, _get, get_1, etc.
haskellFunctionName :: Parser Text
haskellFunctionName = do
  first <- lowerCase <|> underline 
  rest  <- many (digit <|> letter <|> underline) 
  return $ T.pack ([first] ++ rest)

-- Person, Address, PhoneNumber, etc.
haskellTypeName :: Parser Text
haskellTypeName = do
  first <- upperCase
  rest  <- many (digit <|> letter <|> underline) 
  return $ T.pack ([first] ++ rest)


-- underscores, single quotes, letters and digits
-- haskellFunctionName :: 
-- haskellTypeName


spaceNoNewLine :: Parser Char
spaceNoNewLine = satisfy (\x -> isSpace x && not (isEndOfLine x)) <?> "spaceNoNewLine"

skipSpaceNoNewline :: Parser ()
skipSpaceNoNewline = skipWhile (\x -> isSpace x && not (isEndOfLine x))

-- comment functions

parseSingleLineComment :: Parser ()
parseSingleLineComment = do
  string "--"
  _ <- takeTill isEndOfLine
  endOfLine <|> endOfInput
  return ()

parseEmptyLine :: Parser ()
parseEmptyLine = do
  takeWhile (\x -> isSpace x && not (isEndOfLine x))
  endOfLine -- <|> endOfInput
  return ()

parseEmptyLineC :: Parser EntityChild
parseEmptyLineC = do
  parseEmptyLine 
  return $ EntityChildSpace ()

-- multiLineComments are not allowed in model files
{-
parseMultiLineComment :: Parser ()
parseMultiLineComment = do
  string "{-"
  _ <- many' letter <* (string "-}")
  return ()
-}

-- main parsing functions

-- [Entity]

parseEntities :: Parser [Entity]
parseEntities = do
  many' parseEntity


-- Entity

-- order of fieldType, deriving, unique does not matter

parseEntity :: Parser Entity
parseEntity = do
  many' (parseSingleLineComment <|> parseEmptyLine)
  -- many' parseSingleLineComment

  entity <- parseEntityName

  entityChildren <- many' (parseEntityField <|> parseEntityUnique <|> parseEntityDerive <|> parseEmptyLineC)
  
  return $ Entity entity (catEntityFields entityChildren) (catEntityUniques entityChildren) (catEntityChildEntityDerives entityChildren) -- entityFields entityUniques entityDerives

parseEntityName :: Parser EntityName
parseEntityName = do
  name <- haskellTypeName
  -- skipSpaceNoNewline
  rest <- takeTill isEndOfLine
  endOfLine <|> endOfInput
  return $ EntityName name

parseEntityFieldName :: Parser EntityFieldName
parseEntityFieldName = do
  many1 spaceNoNewLine
  name <- haskellFunctionName
  
  case name == "deriving" of
    True -> fail "deriving"
    False -> return $ EntityFieldName name

parseEntityFieldType :: Parser EntityFieldType
parseEntityFieldType = do
  many1 spaceNoNewLine
  mLeftBracket <- maybeOption (char '[')
  name <- haskellTypeName
  
  case mLeftBracket of
    Nothing -> return $ EntityFieldType name False
    Just _  -> do
      char ']'
      return $ EntityFieldType name True
  

parseEntityField :: Parser EntityChild
parseEntityField = do
  efn <- parseEntityFieldName
  eft <- parseEntityFieldType
  rest <- takeTill isEndOfLine
  endOfLine <|> endOfInput

  return $ EntityChildEntityField $ EntityField efn eft rest

-- Unique

parseEntityUniqueName :: Parser EntityUniqueName
parseEntityUniqueName = do
  many1 spaceNoNewLine
  name <- haskellTypeName

  return $ EntityUniqueName name

parseEntityUniqueEntityFieldName :: Parser EntityUniqueEntityFieldName
parseEntityUniqueEntityFieldName = do
  many1 spaceNoNewLine
  name <- haskellFunctionName

  return $ EntityUniqueEntityFieldName name

parseEntityUnique :: Parser EntityChild
parseEntityUnique = do
  eun <- parseEntityUniqueName
  euefn <- parseEntityUniqueEntityFieldName
  rest <- takeTill isEndOfLine
  endOfLine <|> endOfInput

  return $ EntityChildEntityUnique $ EntityUnique eun euefn rest


-- Derive

parseEntityDerive :: Parser EntityChild
parseEntityDerive = do
  many1 spaceNoNewLine
  string "deriving"
  many1 spaceNoNewLine
  name <- haskellTypeName
  -- parseEmptyLine
  rest <- takeTill isEndOfLine
  endOfLine <|> endOfInput

  return $ EntityChildEntityDerive $ EntityDerive $ EntityDeriveType name
