{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.Audit.Generator where

import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T

import           Database.Persist.Audit.Types

generateAuditModels :: [TopLevel] -> Text
generateAuditModels = T.concat . (map printTopLevel)

printTopLevel :: TopLevel -> Text
printTopLevel (TopLevelEntity     e) = ((_getEntityName e) <> "\n")  
                                         <> (T.concat $ map printEntityChild $ _getEntityChildren e)
printTopLevel (TopLevelComment    c) = _getComment c
printTopLevel (TopLevelWhiteSpace w) = _getWhiteSpace w


{-
data Entity = Entity {
  _getEntityName     :: EntityName
, _getEntityChildren :: [EntityChild]
} deriving (Eq,Show,Read)
-}

printEntityChild :: EntityChild -> Text
printEntityChild (EntityChildEntityField  f) = "  " <> _getEntityFieldName f <> "\n"
  -- where
  --  let ec = _getEntityChildEntityFieldText f

printEntityChild (EntityChildEntityUnique u) = ""
printEntityChild (EntityChildEntityDerive d) = ""
printEntityChild (EntityChildComment      c) = _getComment c <> "\n"
printEntityChild (EntityChildWhiteSpace   w) = ""

{-
data EntityChild = EntityChildEntityField  EntityField  |
                   EntityChildEntityUnique EntityUnique |
                   EntityChildEntityDerive EntityDerive |
                   EntityChildComment      Comment      |
                   EntityChildWhiteSpace   WhiteSpace
  deriving (Eq,Show,Read)


data TopLevel    = TopLevelEntity     Entity     |
                   TopLevelComment    Comment    |
                   TopLevelWhiteSpace WhiteSpace  
-}