{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.Audit.ParserSpec ( main
                                         , spec) where

import           Data.Attoparsec.Text
import           Data.Either
import           Data.Text (Text)
import           Database.Persist.Audit.Parser
import           Database.Persist.Audit.Types

import           Test.Hspec                             ( Spec
                                                        , describe
                                                        , hspec
                                                        , it
                                                        , shouldBe
                                                        , shouldMatchList)


{-
Person
  name Text
  phoneNumbers [Text] Maybe
-}

personModel :: Text
personModel = "Person\n  name Text\n  phoneNumbers [Text] Maybe\n"

spec :: Spec
spec = do
  describe "Haskell Function Name Parser" $ do
    -- should pass
    it "Should parse function names starting with an underscore" $ do
      let parseResult = parseOnly haskellFunctionName "_get"
      parseResult `shouldBe` (Right "_get")

    it "Should parse function names starting with a lowercase letter" $ do
      let parseResult = parseOnly haskellFunctionName "get"
      parseResult `shouldBe` (Right "get")

    it "Should parse function names with an numbers" $ do
      let parseResult = parseOnly haskellFunctionName "get1"
      parseResult `shouldBe` (Right "get1")

    it "Should parse function names and ignore the trailing whitespace" $ do
      let parseResult = parseOnly haskellFunctionName "get    "
      parseResult `shouldBe` (Right "get")

    it "Should parse function names, ignore the trailing whitespace, and the things that follow the whitespace" $ do
      let parseResult = parseOnly haskellFunctionName "get    anotherthing"
      parseResult `shouldBe` (Right "get")

    -- should fail
    it "Should not parse inputs that start with whitespace" $ do
      let parseResult = parseOnly haskellFunctionName " _get"
      isLeft parseResult `shouldBe` True

    it "Should not parse function names starting with an uppercase letter" $ do
      let parseResult = parseOnly haskellFunctionName "Get"
      isLeft parseResult `shouldBe` True

    it "Should not parse function names starting with a number" $ do
      let parseResult = parseOnly haskellFunctionName "1get"
      isLeft parseResult `shouldBe` True

    it "Should not parse function names with symbols other than letters, numbers and underscores" $ do
      let parseResults = [ parseOnly haskellFunctionName "get!"
                         , parseOnly haskellFunctionName "get$"
                         , parseOnly haskellFunctionName "get+"
                         , parseOnly haskellFunctionName "get="
                         , parseOnly haskellFunctionName "get-"
                         ]
      (rights parseResults) `shouldMatchList` []

  describe "Haskell Type Name Parser" $ do
    -- should pass
    it "Should parse type names starting with an uppercase letter" $ do
      let parseResult = parseOnly haskellTypeName "Person"
      parseResult `shouldBe` (Right "Person")

    it "Should parse type names starting with an uppercase letter" $ do
      let parseResult = parseOnly haskellTypeName "Person"
      parseResult `shouldBe` (Right "Person")

    -- should fail

    it "Should not parse type names starting with a lowercase letter" $ do
      let parseResult = parseOnly haskellTypeName "person"
      isLeft parseResult `shouldBe` True

    it "Should not parse type names with symbols other than letters, numbers and underscores" $ do
      let parseResults = [ parseOnly haskellTypeName "Get!"
                         , parseOnly haskellTypeName "Get$"
                         , parseOnly haskellTypeName "Get+"
                         , parseOnly haskellTypeName "Get="
                         , parseOnly haskellTypeName "Get-"
                         ]
      (rights parseResults) `shouldMatchList` []

  describe "parseEntityFieldType" $ do
    it "Should parse a well formed entity field type" $ do
      let sampleEntityFieldType = " [Int]"
      let eft = EntityFieldType "Int" True
      let parseResult = parseOnly parseEntityFieldType sampleEntityFieldType
      parseResult `shouldBe` (Right eft) 
      
  describe "parseEntityField" $ do
    it "Should parse a well formed entity field" $ do
      let sampleEntityField = "  ident Text"
      let eft = EntityFieldType "Text" False
      let ef  = EntityField "ident" eft ""
      let parseResult = parseOnly parseEntityField sampleEntityField
      parseResult `shouldBe` (Right ef) 
    
    it "Should parse a well formed entity field" $ do
      let sampleEntityField = "  phoneNumbers [Int]"
      let eft = EntityFieldType "Int" True
      let ef  = EntityField "phoneNumbers" eft ""
      let parseResult = parseOnly parseEntityField sampleEntityField
      parseResult `shouldBe` (Right ef) 
      
    it "Should fail to parse an unindented entity field" $ do
      let sampleEntityField = "ident Text"
      let parseResult = parseOnly parseEntityField sampleEntityField
      isLeft parseResult `shouldBe` True

  describe "parseEntityUnique" $ do
    it "Should parse a well formed entity unique" $ do
      let sampleEntityUnique = "  UniqueGroup group"
      let eu = EntityUnique "UniqueGroup" "group" ""
      let parseResult = parseOnly parseEntityUnique sampleEntityUnique
      parseResult `shouldBe` (Right eu)

  describe "parseEntityDerive" $ do
    it "Should parse a well formed entity derive" $ do
      let sampleEntityDerive = "  deriving Eq"
      let ed = EntityDerive "Eq"
      let parseResult = parseOnly parseEntityDerive sampleEntityDerive
      parseResult `shouldBe` (Right ed)

  describe "parseEntity" $ do
    it "Should parse a well formed entity" $ do
      let sampleEntity = "User\n ident Text\n password Text Maybe\n UniqueUser ident\n deriving Eq\n"
      let parseResult = parseOnly parseEntity sampleEntity

      let eft1 = EntityFieldType "Text" False
      let ef1  = EntityChildEntityField $ EntityField "ident" eft1 ""

      let eft2 = EntityFieldType "Text" False
      let ef2  = EntityChildEntityField $ EntityField "password" eft2 " Maybe"

      let eu = EntityChildEntityUnique $ EntityUnique "UniqueUser" "ident" ""

      let ed = EntityChildEntityDerive $ EntityDerive "Eq"

      let e = Entity "User" [ef1,ef2,eu,ed]

      parseResult `shouldBe` (Right e)

    it "Should parse a well formed entity that has a maybe list data type" $ do
      let parseResult = parseOnly parseEntity personModel
      (isRight parseResult) `shouldBe` True
    
  describe "parsePersistQuasiQuoters" $ do
    it "Should parse a well formed entity" $ do
      let sampleEntity = "[persistLowerCase|User\n ident Text\n password Text Maybe\n UniqueUser ident\n deriving Eq\n|]"
      let parseResult = parseOnly parsePersistQuasiQuoters sampleEntity

      let eft1 = EntityFieldType "Text" False
      let ef1  = EntityChildEntityField $ EntityField "ident" eft1 ""

      let eft2 = EntityFieldType "Text" False
      let ef2  = EntityChildEntityField $ EntityField "password" eft2 " Maybe"

      let eu = EntityChildEntityUnique $ EntityUnique "UniqueUser" "ident" ""

      let ed = EntityChildEntityDerive $ EntityDerive "Eq"

      let e = [PersistModelFileEntity $ Entity "User" [ef1,ef2,eu,ed]]

      parseResult `shouldBe` (Right e)

main :: IO ()
main = hspec spec
