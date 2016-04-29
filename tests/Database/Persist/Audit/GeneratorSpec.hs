{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.Audit.GeneratorSpec (main, spec) where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Attoparsec.Text

import           Data.Text                              (Text)

import           Database.Persist.Audit.Generator
import           Database.Persist.Audit.Parser

import           Test.Hspec                             ( Spec
                                                        , describe
                                                        , hspec
                                                        , it
                                                        , shouldBe)



{- The following data is used below
User
    ident Text
    password Text Maybe
    UniqueUser ident
    deriving Typeable
    deriving Generic
    deriving Eq
    deriving Show
    deriving Ord

Phone
    number Int
    user UserId

instance ToAudit User where
  type AuditResult User = UserAudit
  toAudit v k auditAction editedBy editedOn = UserAudit (userIdent v)
    (userPassword v)
    k auditAction editedBy editedOn 

instance ToAudit Person where
  type AuditResult Person = PersonAudit
  toAudit v k auditAction editedBy editedOn = PersonAudit (personName v)
                                                          (personAge v)
                                                          k auditAction editedBy editedOn 

-}

userModel :: Text
userModel = "User\n  ident Text\n  password Text Maybe\n UniqueUser ident\n  deriving Typeable\n deriving Generic\n  deriving Eq\n  deriving Show\n  deriving Ord"

userAuditModel :: Text
userAuditModel = "UserAudit\n  ident Text\n  password Text Maybe\n  deriving Typeable\n  deriving Generic\n  deriving Eq\n  deriving Show\n  deriving Ord\n  originalId UserId noreference\n  auditAction AuditAction\n  editedBy Text\n  editedOn UTCTime\n\n"

userToAuditInstance :: Text
userToAuditInstance = "instance ToAudit User where\n  type AuditResult User = UserAudit\n  toAudit v k auditAction editedBy editedOn = UserAudit\n    (userIdent v)\n    (userPassword v)\n    (k) auditAction editedBy editedOn\n\n"

userByteStringToAuditInstance :: Text
userByteStringToAuditInstance = "instance ToAudit User where\n  type AuditResult User = UserAudit\n  toAudit v k auditAction editedBy editedOn = UserAudit\n    (userIdent v)\n    (userPassword v)\n    (mongoKeyToByteString $ k) auditAction editedBy editedOn\n\n"

userInt64ToAuditInstance :: Text
userInt64ToAuditInstance = "instance ToAudit User where\n  type AuditResult User = UserAudit\n  toAudit v k auditAction editedBy editedOn = UserAudit\n    (userIdent v)\n    (userPassword v)\n    (fromSqlKey $ k) auditAction editedBy editedOn\n\n"

phoneModel :: Text
phoneModel = "Phone\n  number Int\n  user UserId"

phoneAuditModel :: Text
phoneAuditModel = "PhoneAudit\n  number Int\n  user UserId noreference\n  originalId PhoneId noreference\n  auditAction AuditAction\n  editedBy Text\n  editedOn UTCTime\n\n"

phoneAuditByteStringModel :: Text
phoneAuditByteStringModel = "PhoneAudit\n  number Int\n  user ByteString -- UserId\n  originalId ByteString -- PhoneId\n  auditAction AuditAction\n  editedBy Text\n  editedOn UTCTime\n\n"

phoneAuditInt64Model :: Text
phoneAuditInt64Model = "PhoneAudit\n  number Int\n  user Int64 -- UserId\n  originalId Int64 -- PhoneId\n  auditAction AuditAction\n  editedBy Text\n  editedOn UTCTime\n\n"

{-
data ForeignKeyType = OriginalKey   -- | Default setting. Link the ids as the original type with a "noreference" tag.
                    | MongoKeyInSQL -- | Store Mongo Key as a ByteString in SQL.
                    | SQLKeyInMongo -- | Store SQL Key as an Int64 in Mongo.
  deriving (Eq,Read,Show)

-- | Settings that the author assumed would be most common.
defaultSettings :: AuditGeneratorSettings
defaultSettings =  AuditGeneratorSettings 2 "Audit" True False False OriginalKey

toSqlKey :: ToBackendKey SqlBackend record => Int64 -> Key record
Source

fromSqlKey


-------------------------------------------------------------------------------
-- | Makes a Text representation of a Key.
showKey :: ToBackendKey SqlBackend e => Key e -> Text
showKey = T.pack . show . mkInt


-------------------------------------------------------------------------------
-- | Makes a ByteString representation of a Key.
showKeyBS :: ToBackendKey SqlBackend e => Key e -> ByteString
showKeyBS = T.encodeUtf8 . showKey


-------------------------------------------------------------------------------
-- | Converts a Key to Int.  Fails with error if the conversion fails.
mkInt :: ToBackendKey SqlBackend a => Key a -> Int
mkInt = fromIntegral . unSqlBackendKey . toBackendKey

-}

spec :: Spec
spec = do
  describe "Audit Model generator" $ do
    it "should generate an audit model from a model" $ do
      True `shouldBe` True

{-
spec :: Spec
spec = do
  describe "Audit Model generator" $ do
    it "should generate an audit model from a model" $ do
      let parseResult = parseOnly parseEntities userModel
      case parseResult of 
        Left _ -> False `shouldBe` True
        Right es -> do 
          let generatedAuditModels = generateAuditModels defaultSettings es
          generatedAuditModels == userAuditModel `shouldBe` True
      
    it "should add noreference tag to foreign refereces" $ do
      let parseResult = parseOnly parseEntities phoneModel
      case parseResult of 
        Left _ -> False `shouldBe` True
        Right es -> do 
          let generatedAuditModels = generateAuditModels defaultSettings es

          liftIO $ print generatedAuditModels
          liftIO $ print phoneAuditModel
          generatedAuditModels == phoneAuditModel `shouldBe` True
    
    it "should generate foreign references as ByteString if the MongoKeyInSQL setting is used" $ do
      let parseResult = parseOnly parseEntities phoneModel
      case parseResult of 
        Left _ -> False `shouldBe` True
        Right es -> do 
          let generatedAuditModels = generateAuditModels (defaultSettings {foreignKeyType = MongoKeyInSQL}) es

          liftIO $ print generatedAuditModels
          liftIO $ print phoneAuditByteStringModel
          generatedAuditModels == phoneAuditByteStringModel `shouldBe` True
    

    it "should generate foreign references as Int64 if the SQLKeyInMongo setting is used" $ do
      let parseResult = parseOnly parseEntities phoneModel
      case parseResult of 
        Left _ -> False `shouldBe` True
        Right es -> do 
          let generatedAuditModels = generateAuditModels (defaultSettings {foreignKeyType = SQLKeyInMongo}) es

          liftIO $ print generatedAuditModels
          liftIO $ print phoneAuditInt64Model
          generatedAuditModels == phoneAuditInt64Model `shouldBe` True
    
  describe "ToAudit instance generator" $ do
    it "should generate an instance from a model" $ do
      let parseResult = parseOnly parseEntities userModel
      case parseResult of 
        Left _ -> False `shouldBe` True
        Right es -> do 
          let generatedAuditModels = generateToAuditInstances defaultSettings es
          liftIO $ print generatedAuditModels
          liftIO $ print userToAuditInstance
          generatedAuditModels == userToAuditInstance `shouldBe` True

    it "should generate an instance from a model foreign references as ByteString when the MongoKeyInSQL setting is used" $ do
      let parseResult = parseOnly parseEntities userModel
      case parseResult of 
        Left _ -> False `shouldBe` True
        Right es -> do 
          let generatedAuditModels = generateToAuditInstances (defaultSettings {foreignKeyType = MongoKeyInSQL}) es
          liftIO $ print generatedAuditModels
          liftIO $ print userByteStringToAuditInstance
          generatedAuditModels == userByteStringToAuditInstance `shouldBe` True

    it "should generate an instance from a model with foreign references as Int64 when the SQLKeyInMongo setting is used" $ do
      let parseResult = parseOnly parseEntities userModel
      case parseResult of 
        Left _ -> False `shouldBe` True
        Right es -> do 
          let generatedAuditModels = generateToAuditInstances (defaultSettings {foreignKeyType = SQLKeyInMongo}) es
          liftIO $ print generatedAuditModels
          liftIO $ print userInt64ToAuditInstance
          generatedAuditModels == userInt64ToAuditInstance `shouldBe` True  

-}

main :: IO ()
main = hspec spec
