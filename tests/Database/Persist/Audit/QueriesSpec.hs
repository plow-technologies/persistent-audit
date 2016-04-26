{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Database.Persist.Audit.QueriesSpec (main, spec) where

import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Attoparsec.Text
import           Data.Either
import           Data.Text                              (Text)
import           Data.Time

import           Database.Persist                hiding (Update)
import           Database.Persist.Sqlite         hiding (Update) 
import           Database.Persist.TH

import           Database.Persist.Audit.Class
import           Database.Persist.Audit.Parser
import           Database.Persist.Audit.Queries
import           Database.Persist.Audit.Types

import           Test.Hspec                             ( Spec
                                                        , describe
                                                        , before
                                                        , hspec
                                                        , it
                                                        , shouldBe
                                                        , shouldMatchList)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show

BlogPost
    title String
    authorId PersonId
    deriving Show

PersonAudit
    name String
    age Int Maybe
    deriving Show
    originalId PersonId noreference
    auditAction AuditAction
    editedBy Text
    editedOn UTCTime

BlogPostAudit
    title String
    authorId PersonId noreference
    deriving Show
    originalId BlogPostId noreference
    auditAction AuditAction
    editedBy Text
    editedOn UTCTime
|]


instance ToAudit Person where
  type AuditResult Person = PersonAudit
  toAudit v k auditAction editedBy editedOn = PersonAudit (personName v)
                                                          (personAge v)
                                                          k auditAction editedBy editedOn 

instance ToAudit BlogPost where
  type AuditResult BlogPost = BlogPostAudit
  toAudit v k auditAction editedBy editedOn = BlogPostAudit (blogPostTitle v)
                                                            (blogPostAuthorId v)
                                                            k auditAction editedBy editedOn 

spec :: Spec
spec = do
  describe "Audit Queries" $ do
    it "insertAndAudit should insert the original item and insert an audit version" $ do
      mPair <- liftIO $ runSqlite ":memory:" $ do
        runMigration migrateAll
        johnId <- insertAndAudit (Person "John Doe" $ Just 35) "Admin"
        
        mPerson <- selectFirst [PersonId ==. johnId] [] 
        mPersonAudit <- selectFirst [PersonAuditOriginalId ==. johnId, PersonAuditAuditAction ==. Create] []
        
        return $ (,) <$> mPerson <*> mPersonAudit

      case mPair of
        Nothing -> False `shouldBe` True
        Just (person,personAudit) -> 
          ((personName . entityVal $ person) == (personAuditName . entityVal $ personAudit)) `shouldBe` True

    it "deleteAndAudit should delete the original item and an insert audit version" $ do
      pair <- liftIO $ runSqlite ":memory:" $ do
        runMigration migrateAll
        johnId <- insertAndAudit (Person "John Doe" $ Just 35) "Admin"
        deleteAndAudit johnId "Admin"
        
        mPerson <- selectFirst [PersonId ==. johnId] [] 
        mPersonAudit <- selectList [PersonAuditOriginalId ==. johnId, PersonAuditAuditAction ==. Delete] []
        
        return $ (,) mPerson mPersonAudit

      case fst pair of
        Just _ -> False `shouldBe` True
        Nothing -> (length $ snd pair) == 1 `shouldBe` True
    
    it "updateAndAudit should update the original item and insert an audit version" $ do
      mPersonAudit <- liftIO $ runSqlite ":memory:" $ do
        runMigration migrateAll
        johnId <- insertAndAudit (Person "John Doe" $ Just 35) "Admin"
        
        updateAndAudit johnId [PersonAge =. (Just 30)] "Admin"
        mPersonAudit <- selectFirst [PersonAuditOriginalId ==. johnId, PersonAuditAuditAction ==. Update] []
        
        return mPersonAudit

      case mPersonAudit of
        Nothing -> False `shouldBe` True
        Just _ -> True `shouldBe` True


-- assertFailure
-- Test.HUnit.Lang

main :: IO ()
main = hspec spec
