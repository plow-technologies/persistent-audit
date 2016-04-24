{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.Audit.GeneratorSpec (main, spec) where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Attoparsec.Text

import           Data.Text                              (Text)
import qualified Data.Text                              as T

import           Database.Persist.Audit.Generator
import           Database.Persist.Audit.Parser

import           Test.Hspec                             ( Spec
                                                        , describe
                                                        , hspec
                                                        , it
                                                        , shouldBe)

import           Test.QuickCheck




spec :: Spec
spec = do
  describe "Generator" $ do
    it "Generator" $ do
      let source = "User\n  ident Text\n  password Text Maybe\n UniqueUser ident\n  deriving Typeable\n deriving Generic\n  deriving Eq\n  deriving Show\n  deriving Ord"
      let output = "UserHistory\n  ident Text\n  password Text Maybe\n  deriving Typeable\n  deriving Generic\n  deriving Eq\n  deriving Show\n  deriving Ord\n  originalId PersistValue\n  deleted Bool\n  editedBy Text\n  editedOn UTCTime\n\n"
      
      let parseResult = parseOnly parseEntities source
      case parseResult of 
        Left _ -> False `shouldBe` True
        Right es -> do 
          let generatedAuditModels = generateAuditModels defaultSettings es
          -- liftIO $ print generatedAuditModels
          -- liftIO $ print output
          generatedAuditModels == output `shouldBe` True

{-
User
    ident Text
    password Text Maybe
    UniqueUser ident
    deriving Typeable
    deriving Generic
    deriving Eq
    deriving Show
    deriving Ord
-}



main :: IO ()
main = hspec spec
