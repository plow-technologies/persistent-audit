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

emptyModel :: Text
emptyModel = "Test json sql=testing\n"

emptyAuditModel :: Text
emptyAuditModel = "TestAudit json sql=testing\n  originalId TestId noreference\n  auditAction AuditAction\n  editedBy Text\n  editedOn UTCTime\n\n"

userModel :: Text
userModel = "User\n  ident Text\n  password Text Maybe\n UniqueUser ident\n  deriving Typeable\n deriving Generic\n  deriving Eq\n  deriving Show\n  deriving Ord"

userAuditModel :: Text
userAuditModel = "UserAudit\n  ident Text\n  password Text Maybe\n  deriving Typeable\n  deriving Generic\n  deriving Eq\n  deriving Show\n  deriving Ord\n  originalId UserId noreference\n  auditAction AuditAction\n  editedBy Text\n  editedOn UTCTime\n\n"

userToAuditInstance :: Text
userToAuditInstance = "instance ToAudit User where\n  type AuditResult User = UserAudit\n  toAudit v k auditAction editedBy editedOn = UserAudit\n    (userIdent v)\n    (userPassword v)\n    (k) auditAction editedBy editedOn\n\n"

userToAuditInstanceByteString :: Text
userToAuditInstanceByteString = "instance ToAudit User where\n  type AuditResult User = UserAudit\n  toAudit v k auditAction editedBy editedOn = UserAudit\n    (userIdent v)\n    (userPassword v)\n    (mongoKeyToByteString k) auditAction editedBy editedOn\n\n"

userToAuditInstanceInt64 :: Text
userToAuditInstanceInt64 = "instance ToAudit User where\n  type AuditResult User = UserAudit\n  toAudit v k auditAction editedBy editedOn = UserAudit\n    (userIdent v)\n    (userPassword v)\n    (fromSqlKey k) auditAction editedBy editedOn\n\n"



phoneModel :: Text
phoneModel = "Phone\n  number Int\n  user UserId"

phoneAuditModel :: Text
phoneAuditModel = "PhoneAudit\n  number Int\n  user UserId noreference\n  originalId PhoneId noreference\n  auditAction AuditAction\n  editedBy Text\n  editedOn UTCTime\n\n"

phoneToAuditInstanceByteString :: Text
phoneToAuditInstanceByteString = "instance ToAudit Phone where\n  type AuditResult Phone = PhoneAudit\n  toAudit v k auditAction editedBy editedOn = PhoneAudit\n    (phoneNumber v)\n    (mongoKeyToByteString $ phoneUser v)\n    (mongoKeyToByteString k) auditAction editedBy editedOn\n\n"



phoneWithMaybeModel :: Text
phoneWithMaybeModel = "Phone\n  number Int\n  user UserId Maybe"

phoneWithMaybeToAuditInstanceByteString :: Text
phoneWithMaybeToAuditInstanceByteString = "instance ToAudit Phone where\n  type AuditResult Phone = PhoneAudit\n  toAudit v k auditAction editedBy editedOn = PhoneAudit\n    (phoneNumber v)\n    (mongoKeyToByteString <$> phoneUser v)\n    (mongoKeyToByteString k) auditAction editedBy editedOn\n\n"

-- userToAuditInstanceInt64 :: Text
-- userToAuditInstanceInt64 = "instance ToAudit User where\n  type AuditResult User = UserAudit\n  toAudit v k auditAction editedBy editedOn = UserAudit\n    (userIdent v)\n    (userPassword v)\n    (fromSqlKey k) auditAction editedBy editedOn\n\n"

phoneAuditByteStringModel :: Text
phoneAuditByteStringModel = "PhoneAudit\n  number Int\n  user ByteString -- UserId\n  originalId ByteString -- PhoneId\n  auditAction AuditAction\n  editedBy Text\n  editedOn UTCTime\n\n"

phoneAuditInt64Model :: Text
phoneAuditInt64Model = "PhoneAudit\n  number Int\n  user Int64 -- UserId\n  originalId Int64 -- PhoneId\n  auditAction AuditAction\n  editedBy Text\n  editedOn UTCTime\n\n"


addressModel :: Text
addressModel = "Address\n  address Text\n  phoneNumbers [Phone]"

addressAuditModel :: Text
addressAuditModel = "AddressAudit\n  address Text\n  phoneNumbers [Phone]\n  originalId AddressId noreference\n  auditAction AuditAction\n  editedBy Text\n  editedOn UTCTime\n\n"


spec :: Spec
spec = do
  describe "Audit Model generator" $ do
    it "should generate an audit model from a model" $ do
      let parseResult = parseOnly parseEntities emptyModel
      case parseResult of 
        Left _ -> False `shouldBe` True
        Right es -> do 
          let generatedAuditModels = generateAuditModels defaultSettings es
          generatedAuditModels  `shouldBe` emptyAuditModel

    it "should generate an audit model for another model" $ do
      let parseResult = parseOnly parseEntities userModel
      case parseResult of 
        Left _ -> False `shouldBe` True
        Right es -> do 
          let generatedAuditModels = generateAuditModels defaultSettings es
          generatedAuditModels `shouldBe` userAuditModel

    it "should add noreference tag to foreign refereces" $ do
      let parseResult = parseOnly parseEntities phoneModel
      case parseResult of 
        Left _ -> False `shouldBe` True
        Right es -> do 
          let generatedAuditModels = generateAuditModels defaultSettings es
          generatedAuditModels `shouldBe` phoneAuditModel

    it "should generate fields that are list types" $ do
      let parseResult = parseOnly parseEntities addressModel
      case parseResult of 
        Left _ -> False `shouldBe` True
        Right es -> do
          liftIO $ print es 
          let generatedAuditModels = generateAuditModels defaultSettings es
          generatedAuditModels `shouldBe` addressAuditModel
    
    it "should generate foreign references as ByteString if the MongoKeyInSQL setting is used" $ do
      let parseResult = parseOnly parseEntities phoneModel
      case parseResult of 
        Left _ -> False `shouldBe` True
        Right es -> do 
          let generatedAuditModels = generateAuditModels (defaultSettings {foreignKeyType = MongoKeyInSQL}) es
          generatedAuditModels `shouldBe` phoneAuditByteStringModel

    it "should generate foreign references as Int64 if the SQLKeyInMongo setting is used" $ do
      let parseResult = parseOnly parseEntities phoneModel
      case parseResult of 
        Left _ -> False `shouldBe` True
        Right es -> do 
          let generatedAuditModels = generateAuditModels (defaultSettings {foreignKeyType = SQLKeyInMongo}) es
          generatedAuditModels `shouldBe` phoneAuditInt64Model



  describe "ToAudit instance generator" $ do
    it "should generate an instance from a model" $ do
      let parseResult = parseOnly parseEntities userModel
      case parseResult of 
        Left _ -> False `shouldBe` True
        Right es -> do 
          let generatedAuditInstances = generateToAuditInstances defaultSettings es
          generatedAuditInstances `shouldBe` userToAuditInstance

    it "should generate an instance from a model foreign references as ByteString when the MongoKeyInSQL setting is used" $ do
      let parseResult = parseOnly parseEntities phoneModel
      case parseResult of 
        Left _ -> False `shouldBe` True
        Right es -> do 
          let generatedAuditInstances = generateToAuditInstances (defaultSettings {foreignKeyType = MongoKeyInSQL}) es
          generatedAuditInstances `shouldBe` phoneToAuditInstanceByteString

    it "should generate an instance from a model maybe foreign references as fmap ByteString when the MongoKeyInSQL setting is used" $ do
      let parseResult = parseOnly parseEntities phoneWithMaybeModel
      case parseResult of 
        Left _ -> False `shouldBe` True
        Right es -> do 
          let generatedAuditInstances = generateToAuditInstances (defaultSettings {foreignKeyType = MongoKeyInSQL}) es
          generatedAuditInstances `shouldBe` phoneWithMaybeToAuditInstanceByteString
    
    it "should generate an instance from a model with foreign references as Int64 when the SQLKeyInMongo setting is used" $ do
      let parseResult = parseOnly parseEntities userModel
      case parseResult of 
        Left _ -> False `shouldBe` True
        Right es -> do 
          let generatedAuditInstances = generateToAuditInstances (defaultSettings {foreignKeyType = SQLKeyInMongo}) es
          liftIO $ print parseResult
          generatedAuditInstances `shouldBe` userToAuditInstanceInt64


main :: IO ()
main = hspec spec
