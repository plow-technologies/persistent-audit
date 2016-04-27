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

instance ToAudit user where
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

userToAuditInstance :: Text
userToAuditInstance = "instance ToAudit User where\n  type AuditResult User = UserAudit\n  toAudit v k auditAction editedBy editedOn = UserAudit\n    (userIdent v)\n    (userPassword v)\n    k auditAction editedBy editedOn\n"

userAuditModel :: Text
userAuditModel = "UserAudit\n  ident Text\n  password Text Maybe\n  deriving Typeable\n  deriving Generic\n  deriving Eq\n  deriving Show\n  deriving Ord\n  originalId UserId noreference\n  auditAction AuditAction\n  editedBy Text\n  editedOn UTCTime\n\n"

spec :: Spec
spec = do
  describe "Audit Model generator" $ do
    it "should generate an audit model from a model" $ do
      let parseResult = parseOnly parseEntities userModel
      case parseResult of 
        Left _ -> False `shouldBe` True
        Right es -> do 
          let generatedAuditModels = generateAuditModels defaultSettings es
          -- liftIO $ print generatedAuditModels
          -- liftIO $ print output
          generatedAuditModels == userAuditModel `shouldBe` True
  
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
  


main :: IO ()
main = hspec spec
