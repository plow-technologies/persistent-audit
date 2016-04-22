{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Persist.Audit.Parser where

import           Control.Applicative 

import           Data.Attoparsec.ByteString.Char8 (isSpace)
import           Data.Attoparsec.Text

import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T

import           Database.Persist.Audit.Types

import           Prelude hiding (takeWhile)



{- Persist Model

-- TopLevelWhiteSpace above
-- TopLevelComment
User   -- TopLevelEntity
  name     Text -- EntityLevelEntityField

  -- EntityLevelWhiteSpace
  -- EntityLevelComment
  UniqueUser name -- EntityLevelEntityUnique
  deriving Eq     -- EntityLevelEntityDerive
-}
  
-- helper functions

maybeOption :: Parser a -> Parser (Maybe a)
maybeOption p = option Nothing (Just <$> p)

lowerCase :: Parser Char
lowerCase = satisfy (\char -> char >= 'a' && char <= 'z')

upperCase :: Parser Char
upperCase = satisfy (\char -> char >= 'A' && char <= 'Z')

underline :: Parser Char 
underline = satisfy (== '_')

spaceNoNewLine :: Parser Char
spaceNoNewLine = satisfy (\x -> isSpace x && not (isEndOfLine x)) <?> "spaceNoNewLine"

-- starts with underscore or lowercase letter
-- underscores, single quotes, letters and digits
-- get, _get, get_1, etc.
haskellFunctionName :: Parser Text
haskellFunctionName = do
  first <- lowerCase <|> underline 
  rest  <- many (digit <|> letter <|> underline) 
  return $ T.pack ([first] ++ rest)

-- starts with uppercase letter
-- Person, Address, PhoneNumber, etc.
haskellTypeName :: Parser Text
haskellTypeName = do
  first <- upperCase
  rest  <- many (digit <|> letter <|> underline) 
  return $ T.pack ([first] ++ rest)

-- comment functions

singleLineComment :: Parser Comment
singleLineComment = do
  string "--"
  comment <- takeTill isEndOfLine
  endOfLine
  return $ Comment ("--" <> comment <> "\n")


collectWhiteSpace :: Parser Text
collectWhiteSpace = do
  whiteSpace <- takeWhile (\x -> isSpace x && not (isEndOfLine x))
  endOfLine -- <|> endOfInput
  return $ whiteSpace


collectWhiteSpaceX :: Parser WhiteSpace
collectWhiteSpaceX = do
  whiteSpace <- takeWhile (\x -> isSpace x && not (isEndOfLine x))
  endOfLine -- <|> endOfInput
  return $ WhiteSpace (whiteSpace <> "\n")



-- main parsing functions

-- [Entity]

parseEntities :: Parser [TopLevel]
parseEntities = do
  many' ( TopLevelEntity     <$> parseEntity 
      <|> TopLevelWhiteSpace <$> collectWhiteSpaceX
      <|> TopLevelComment    <$> singleLineComment)


-- Entity

-- order of fieldType, deriving, unique does not matter

parseEntity :: Parser Entity
parseEntity = do

  entityName <- parseEntityName
  entityChildren <- many' ( EntityChildEntityField  <$> parseEntityField 
                        <|> EntityChildEntityUnique <$> parseEntityUnique 
                        <|> EntityChildEntityDerive <$> parseEntityDerive 
                        <|> EntityChildWhiteSpace   <$> collectWhiteSpaceX
                        <|> EntityChildComment      <$> singleLineComment)
  
  return $ Entity entityName entityChildren

-- EntityName

parseEntityName :: Parser Text
parseEntityName = do
  name <- haskellTypeName
  rest <- takeTill isEndOfLine
  endOfLine <|> endOfInput
  return name

-- EntityField

parseEntityField :: Parser EntityField
parseEntityField = do
  efn <- parseEntityFieldName
  eft <- parseEntityFieldType
  rest <- takeTill isEndOfLine
  endOfLine <|> endOfInput

  return $ EntityField efn eft rest

parseEntityFieldName :: Parser Text
parseEntityFieldName = do
  many1 spaceNoNewLine
  name <- haskellFunctionName
  
  case name == "deriving" of
    True -> fail "deriving"
    False -> return name

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
  
-- EntityUnique

parseEntityUnique :: Parser EntityUnique
parseEntityUnique = do
  eun <- parseEntityUniqueName
  euefn <- parseEntityUniqueEntityFieldName
  rest <- takeTill isEndOfLine
  endOfLine <|> endOfInput

  return $ EntityUnique eun euefn rest

parseEntityUniqueName :: Parser Text
parseEntityUniqueName = do
  many1 spaceNoNewLine
  haskellTypeName

parseEntityUniqueEntityFieldName :: Parser Text
parseEntityUniqueEntityFieldName = do
  many1 spaceNoNewLine
  haskellFunctionName
  

-- EntityDerive

parseEntityDerive :: Parser EntityDerive
parseEntityDerive = do
  many1 spaceNoNewLine
  string "deriving"
  many1 spaceNoNewLine
  name <- haskellTypeName
  
  rest <- takeTill isEndOfLine
  endOfLine <|> endOfInput

  return $ EntityDerive name
