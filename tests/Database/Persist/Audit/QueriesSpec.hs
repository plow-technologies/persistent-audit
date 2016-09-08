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

import           Control.Monad.IO.Class

import           Data.Maybe
import           Data.Text                              (Text)
import           Data.Time

import           Database.Persist                hiding (Update)
import           Database.Persist.Sqlite         hiding (Update)
import           Database.Persist.TH

import           Database.Persist.Audit.Class
import           Database.Persist.Audit.Queries
import           Database.Persist.Audit.Types

import           Test.Hspec

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    UniqueName name
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

    it "insertUniqueAndAudit should insert the original item and insert an audit version" $ do
      mPair <- liftIO $ runSqlite ":memory:" $ do
        runMigration migrateAll
        mJaneId <- insertUniqueAndAudit (Person "Jane Doe" $ Just 35) "Admin"
        case mJaneId of
          Nothing -> return Nothing
          Just janeId -> do
            mPerson <- selectFirst [PersonId ==. janeId] []
            mPersonAudit <- selectFirst [PersonAuditOriginalId ==. janeId, PersonAuditAuditAction ==. Create] []
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

    it "deleteWhereAndAudit" $ do
      (people,peopleAudit) <- liftIO $ runSqlite ":memory:" $ do
        runMigration migrateAll
        _ <- insertAndAudit (Person "John Doe" $ Just 35) "deleteWhereAndAudit"
        _ <- insertAndAudit (Person "Jane Doe" $ Just 35) "deleteWhereAndAudit"
        deleteWhereAndAudit [PersonAge ==. Just 35] "deleteWhereAndAudit"

        people <- selectList [PersonAge ==. Just 35] []
        peopleAudit <- selectList [PersonAuditAge ==. Just 35, PersonAuditAuditAction ==. Delete, PersonAuditEditedBy ==. "deleteWhereAndAudit"] []
        return (people,peopleAudit)

      length people `shouldBe` 0
      length peopleAudit `shouldBe` 2

    it "deleteByAndAudit" $ do
      (people,peopleAudit) <- liftIO $ runSqlite ":memory:" $ do
        runMigration migrateAll
        _ <- insertAndAudit (Person "John Doe" $ Just 35) "deleteByAndAudit"
        deleteByAndAudit (UniqueName "John Doe") "deleteByAndAudit"

        people <- selectList [PersonName ==. "John Doe"] []
        peopleAudit <- selectList [PersonAuditName ==. "John Doe", PersonAuditAuditAction ==. Delete, PersonAuditEditedBy ==. "deleteByAndAudit"] []
        return (people,peopleAudit)

      length people `shouldBe` 0
      length peopleAudit `shouldBe` 1

    it "updateWhereAndAudit" $ do
      mPersonAudit <- liftIO $ runSqlite ":memory:" $ do
        runMigration migrateAll
        johnId <- insertAndAudit (Person "John Doe" $ Just 35) "Admin"

        updateWhereAndAudit [PersonName ==. "John Doe"] [PersonAge =. (Just 30)] "Admin"
        mPersonAudit <- selectFirst [PersonAuditOriginalId ==. johnId, PersonAuditAuditAction ==. Update] []

        return mPersonAudit

      isJust mPersonAudit `shouldBe` True

    it "repsertAndAudit" $ do
      (person,personAudits) <- liftIO $ runSqlite ":memory:" $ do
        runMigration migrateAll
        johnId <- insertAndAudit (Person "John Doe" $ Just 35) "Admin"
        _      <- repsertAndAudit johnId (Person "John Smith" $ Just 30) "Admin"

        person <- get johnId
        personAudits <- selectList [PersonAuditOriginalId ==. johnId, PersonAuditAuditAction ==. Update] []
        return $ (person,personAudits)

      personName <$> person `shouldBe` Just "John Smith"
      personAge <$> person `shouldBe` Just (Just 30)
      (Just . personAuditName . entityVal . head $ personAudits) `shouldBe` (personName <$> person)

    it "replaceAndAudit" $ do
      (person,personAudits) <- liftIO $ runSqlite ":memory:" $ do
        runMigration migrateAll
        johnId <- insertAndAudit (Person "John Doe" $ Just 35) "Admin"
        _      <- replaceAndAudit johnId (Person "John Smith" $ Just 30) "Admin"

        person <- get johnId
        personAudits <- selectList [PersonAuditOriginalId ==. johnId, PersonAuditAuditAction ==. Update] []
        return $ (person,personAudits)

      personName <$> person `shouldBe` Just "John Smith"
      personAge <$> person `shouldBe` Just (Just 30)
      (Just . personAuditName . entityVal . head $ personAudits) `shouldBe` (personName <$> person)

main :: IO ()
main = hspec spec
