{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.Audit.ParserSpec ( main
                                         , spec) where

import           Data.Attoparsec.Text
import           Data.Either

import           Database.Persist.Audit.Parser
import           Database.Persist.Audit.Parser.Types
import           Database.Persist.Audit.Types

import           Test.Hspec                             ( Spec
                                                        , describe
                                                        , hspec
                                                        , it
                                                        , shouldBe
                                                        , shouldMatchList)



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

    it "Should parse strict type names" $ do
      let parseResult = parseOnly haskellTypeName "!Get"
      parseResult `shouldBe` (Right "Get")
    
      -- parseResult `shouldBe` (Right "!Get")
    
    it "Should parse lazy type names" $ do
      let parseResult = parseOnly haskellTypeName "~Get"
      parseResult `shouldBe` (Right "Get")
    
    

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

  describe "parseEntityPrimary" $ do
    it "should parse" $ do
      let parseResult = parseOnly parseEntityPrimary "  Primary name age"
      parseResult `shouldBe` (Right (EntityPrimary ["name","age"]))
  
  describe "parseEntityForeign" $ do
    it "should parse" $ do
      let parseResult = parseOnly parseEntityForeign "  Foreign Tree fkparent parent"
      parseResult `shouldBe` (Right (EntityForeign "Tree" ["fkparent","parent"]))
  

  describe "parseMigrationOnlyAndSafeToRemove" $ do

    it "Should parse MigrationOnly" $ do
      let parseResult = parseOnly (parseMigrationOnlyAndSafeToRemove []) "     MigrationOnly"
      parseResult `shouldBe` (Right [MigrationOnly])

    it "Should parse SafeToRemove" $ do
      let parseResult = parseOnly (parseMigrationOnlyAndSafeToRemove []) "   SafeToRemove"
      parseResult `shouldBe` (Right [SafeToRemove]) 

    it "Should parse empty space" $ do
      let parseResult = parseOnly (parseMigrationOnlyAndSafeToRemove []) "     "
      parseResult `shouldBe` (Right [])

    it "Should parse MigrationOnly and SafeToRemove" $ do
      let parseResult = parseOnly (parseMigrationOnlyAndSafeToRemove []) "  MigrationOnly   SafeToRemove"
      parseResult `shouldBe` (Right [MigrationOnly,SafeToRemove]) 

    it "Should parse SafeToRemove and MigrationOnly" $ do
      let parseResult = parseOnly (parseMigrationOnlyAndSafeToRemove []) "   SafeToRemove  MigrationOnly"
      parseResult `shouldBe` (Right [SafeToRemove,MigrationOnly]) 

  describe "parseEntityFieldLastItem" $ do
    it "Should parse a default assignment" $ do
      let parseResult = parseOnly (parseEntityFieldLastItem []) "   default  =   Nothing"
      parseResult `shouldBe` (Right [(FieldDefault "Nothing")]) 

    it "Should parse an sql row assignment" $ do
      let parseResult = parseOnly (parseEntityFieldLastItem []) "   sql=person"
      parseResult `shouldBe` (Right [(FieldSqlRow "person")])

    it "Should parse an sql type assignment" $ do
      let parseResult = parseOnly (parseEntityFieldLastItem []) "   sqltype=date"
      parseResult `shouldBe` (Right [(FieldSqlType "date")])

    it "Should parse an maxlen assignment" $ do
      let parseResult = parseOnly (parseEntityFieldLastItem []) "   maxlen=15"
      parseResult `shouldBe` (Right [(FieldMaxLen 15)])

    it "Should parse mulitple items" $ do
      let parseResult = parseOnly (parseEntityFieldLastItem []) "   default  =   Nothing maxlen=15"
      parseResult `shouldBe` (Right [(FieldDefault "Nothing"), (FieldMaxLen 15)])
    
    it "Should parse mulitple items" $ do
      let parseResult = parseOnly (parseEntityFieldLastItem []) "   default  =   Nothing maxlen=15  sqltype=date   sql=person"
      parseResult `shouldBe` (Right [(FieldDefault "Nothing"), (FieldMaxLen 15),(FieldSqlType "date"), (FieldSqlRow "person")])
      

  describe "parseEntity" $ do
    it "parses Entity with only a table name" $ do
      let parseResult = parseOnly (parseEntity) "Person"
      parseResult `shouldBe` (Right (Entity "Person" False Nothing []))

    it "parses Entity with table name and derive json" $ do
      let parseResult = parseOnly (parseEntity) "Person   json"
      parseResult `shouldBe` (Right (Entity "Person" True Nothing []))
    
    it "parses Entity with table name and sql table set to person" $ do
      let parseResult = parseOnly (parseEntity) "Person sql  =  person"
      parseResult `shouldBe` (Right (Entity "Person" False (Just "person") []))
    
    it "parses Entity with table name, derive json, and table set to person" $ do
      let parseResult = parseOnly (parseEntity) "Person   json     sql=   person"
      parseResult `shouldBe` (Right (Entity "Person" True (Just "person") []))
    
    it "parses Entity with table name and one field" $ do
      let parseResult = parseOnly (parseEntity) "Person\n  name Text"
      parseResult `shouldBe` (Right (Entity "Person" False Nothing [(EntityChildEntityField $ EntityField "name" (EntityFieldType "Text" Strict False False) False False Nothing Nothing Nothing Nothing)]))
    
    it "parses Entity with a table name,  multiple fields, Unique, Foreign and deriving" $ do
      let parseResult = parseOnly (parseEntity) "Person\n  name Text\n  phoneNumber Int Maybe\n  friends [PersonId]\n  UniquePerson name\n  deriving Eq Read Show\n  Primary name age"
      parseResult `shouldBe` (Right (Entity "Person" False Nothing [(EntityChildEntityField  $ EntityField   "name" (EntityFieldType "Text" Strict False False) False False Nothing Nothing Nothing Nothing)
                                                                   ,(EntityChildEntityField  $ EntityField   "phoneNumber" (EntityFieldType "Int" Strict False True) False False Nothing Nothing Nothing Nothing)
                                                                   ,(EntityChildEntityField  $ EntityField   "friends" (EntityFieldType "PersonId" Strict True False) False False Nothing Nothing Nothing Nothing)
                                                                   ,(EntityChildEntityUnique $ EntityUnique "UniquePerson" ["name"])
                                                                   ,(EntityChildEntityDerive $ EntityDerive ["Eq","Read","Show"])
                                                                   ,(EntityChildEntityPrimary $ EntityPrimary ["name","age"])]))
    
      


  describe "parseEntityField" $ do
    it "Should parse a well formed entity field" $ do
      let sampleEntityField = "  ident Text"
      let eft = EntityFieldType "Text" Strict False False
      let ef  = EntityField "ident" eft  False False Nothing Nothing Nothing Nothing
      let parseResult = parseOnly parseEntityField sampleEntityField
      parseResult `shouldBe` (Right ef) 
    
    it "Should parse a well formed entity field with list type" $ do
      let sampleEntityField = "  ident [Text]"
      let eft = EntityFieldType "Text" Strict True False
      let ef  = EntityField "ident" eft  False False Nothing Nothing Nothing Nothing
      let parseResult = parseOnly parseEntityField sampleEntityField
      parseResult `shouldBe` (Right ef) 
    
    it "Should parse a well formed entity field with maybe type" $ do
      let sampleEntityField = "  ident Text Maybe"
      let eft = EntityFieldType "Text" Strict False True
      let ef  = EntityField "ident" eft  False False Nothing Nothing Nothing Nothing
      let parseResult = parseOnly parseEntityField sampleEntityField
      parseResult `shouldBe` (Right ef) 
    
    it "Should parse a well formed entity field with maybe list type" $ do
      let sampleEntityField = "  ident [Text] Maybe"
      let eft = EntityFieldType "Text" Strict True True
      let ef  = EntityField "ident" eft  False False Nothing Nothing Nothing Nothing
      let parseResult = parseOnly parseEntityField sampleEntityField
      parseResult `shouldBe` (Right ef)


    it "Should parse an entity field with explicit strict type" $ do
      let sampleEntityField = "  ident !Text"
      let eft = EntityFieldType "Text" ExplicitStrict False False 
      let ef  = EntityField "ident" eft  False False Nothing Nothing Nothing Nothing
      let parseResult = parseOnly parseEntityField sampleEntityField
      parseResult `shouldBe` (Right ef) 

    it "Should parse an entity field with a lazy type" $ do
      let sampleEntityField = "  ident ~Text"
      let eft = EntityFieldType "Text" Lazy False False
      let ef  = EntityField "ident" eft  False False Nothing Nothing Nothing Nothing
      let parseResult = parseOnly parseEntityField sampleEntityField
      parseResult `shouldBe` (Right ef) 

    it "Should parse an entity field with a list of lazy types" $ do
      let sampleEntityField = "  ident [~Text]"
      let eft = EntityFieldType "Text" Lazy True False
      let ef  = EntityField "ident" eft  False False Nothing Nothing Nothing Nothing
      let parseResult = parseOnly parseEntityField sampleEntityField
      parseResult `shouldBe` (Right ef)  
    
main :: IO ()
main = hspec spec
