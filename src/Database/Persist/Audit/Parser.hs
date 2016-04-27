{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Persist.Audit.Parser where

import           Control.Applicative 

import           Data.Attoparsec.ByteString.Char8 (isSpace)
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text

import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T

import           Database.Persist.Audit.Types

import           Prelude hiding (takeWhile)


parseQuasiQuoterFile :: Text -> Either String PersistModelFile
parseQuasiQuoterFile = parseOnly parseEntities

parseModelsFile :: Text -> Either String PersistModelFile
parseModelsFile = parseOnly parseEntities

-- | Parse Persist Models that are in quasi-quoters. The source could be a haskell file.
parsePersistQuasiQuoters :: Parser PersistModelFile
parsePersistQuasiQuoters = do
  _ <- manyTill' anyChar (string "[persistLowerCase|" <|> string "[persistUpperCase|")
  manyTill' ( PersistModelFileEntity     <$> parseEntity 
      <|> PersistModelFileWhiteSpace <$> collectWhiteSpace
      <|> PersistModelFileComment    <$> singleLineComment) (string "|]")

-- | Parse a Persist Model file.
parseEntities :: Parser PersistModelFile
parseEntities = do
  many' ( PersistModelFileEntity     <$> parseEntity 
      <|> PersistModelFileWhiteSpace <$> collectWhiteSpace
      <|> PersistModelFileComment    <$> singleLineComment)

-- | Parse a single Persist Entity
parseEntity :: Parser Entity
parseEntity = do

  entityName <- parseEntityName
  entityChildren <- many' ( EntityChildEntityField  <$> parseEntityField 
                        <|> EntityChildEntityUnique <$> parseEntityUnique 
                        <|> EntityChildEntityDerive <$> parseEntityDerive 
                        <|> EntityChildWhiteSpace   <$> collectWhiteSpace
                        <|> EntityChildComment      <$> singleLineComment)
  
  return $ Entity entityName entityChildren

  
-- helper functions

-- | Wrap a Parser in 'Maybe' because it might fail. Useful for making choices.
maybeOption :: Parser a -> Parser (Maybe a)
maybeOption p = option Nothing (Just <$> p)

-- | Parse a lowercase 'Char'.
lowerCase :: Parser Char
lowerCase = satisfy (\c -> c >= 'a' && c <= 'z')

-- | Parse an uppercase 'Char'.
upperCase :: Parser Char
upperCase = satisfy (\c -> c >= 'A' && c <= 'Z')

-- | Parse an underline.
underline :: Parser Char 
underline = satisfy (== '_')

-- | Parse any space 'Char' excluding "\n".
spaceNoNewLine :: Parser Char
spaceNoNewLine = satisfy (\x -> isSpace x && not (isEndOfLine x)) <?> "spaceNoNewLine"

-- | Parse a Haskell function name. It starts with underscore or lowercase letter then 
-- is followed by a combination of underscores, single quotes, letters and digits.
-- E.g., "get", "_get", "get_1", etc.
haskellFunctionName :: Parser Text
haskellFunctionName = do
  first <- lowerCase <|> underline 
  rest  <- many' (digit <|> letter <|> underline) 
  lookAhead ((space *> pure ()) <|> (char ']' *> pure ()) <|> endOfInput)
  return $ T.pack ([first] ++ rest)

-- | Parse a Haskell type name. It starts with an uppercase letter then 
-- is followed by a combination of underscores, single quotes, letters and digits.
-- E.g., "Person", "Address", "PhoneNumber", etc.
haskellTypeName :: Parser Text
haskellTypeName = do
  first <- upperCase
  rest  <- many' (digit <|> letter <|> underline) 
  -- check for ']' because it could be in a list
  lookAhead ((space *> pure ()) <|> (char ']' *> pure ())  <|> endOfInput )
  return $ T.pack ([first] ++ rest)

-- | Parse a comment that starts with "-- ".
singleLineComment :: Parser Comment
singleLineComment = do
  _ <- string "--"
  comment <- takeTill isEndOfLine
  endOfLine
  return $ Comment ("--" <> comment <> "\n")


collectWhiteSpace :: Parser WhiteSpace
collectWhiteSpace = do
  whiteSpace <- takeWhile (\x -> isSpace x && not (isEndOfLine x))
  endOfLine -- <|> endOfInput
  return $ WhiteSpace (whiteSpace <> "\n")



-- main parsing functions

-- [Entity]


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
  _ <- many1 spaceNoNewLine
  name <- haskellFunctionName
  
  case name == "deriving" of
    True -> fail "deriving"
    False -> return name

parseEntityFieldType :: Parser EntityFieldType
parseEntityFieldType = do
  _ <- many1 spaceNoNewLine
  mLeftBracket <- maybeOption (char '[')
  name <- haskellTypeName
  
  case mLeftBracket of
    Nothing -> return $ EntityFieldType name False
    Just _  -> do
      _ <- char ']'
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
  _ <- many1 spaceNoNewLine
  haskellTypeName

parseEntityUniqueEntityFieldName :: Parser Text
parseEntityUniqueEntityFieldName = do
  _ <- many1 spaceNoNewLine
  haskellFunctionName
  

-- EntityDerive

parseEntityDerive :: Parser EntityDerive
parseEntityDerive = do
  _ <- many1 spaceNoNewLine
  _ <- string "deriving"
  _ <- many1 spaceNoNewLine
  name <- haskellTypeName
  
  rest <- takeTill isEndOfLine
  endOfLine <|> endOfInput

  return $ EntityDerive name


parseForeignKeyType :: Parser () -- Text
parseForeignKeyType = do
  _ <- manyTill anyChar (string "Id" *> endOfInput)
  return ()
-- simpleComment   = string "<!--" *> manyTill' anyChar (string "-->")
-- endOfInput

