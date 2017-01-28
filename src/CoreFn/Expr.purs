-- |
-- The core functional representation
--
module CoreFn.Expr
  ( Bind(..)
  , Expr(..)
  , CaseAlternative(..)
  , Guard
  , readBindJSON
  , readExprJSON
  ) where

import Prelude
import Data.Foreign (F, Foreign, ForeignError(..), fail, parseJSON, readString)
import Data.Foreign.Class (class IsForeign, read, readProp)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Generic (class Generic, gShow)

import CoreFn.Ident (Ident(..), readIdent)
import CoreFn.Names (Qualified)
import CoreFn.Util (objectProps)
import CoreFn.Literals (Literal)
import CoreFn.Binders (Binder)

-- |
-- Data type for expressions and terms
--
data Expr
  -- |
  -- A literal value
  --
  = Literal (Literal Expr)
  -- |
  -- Function introduction
  --
  | Abs Ident Expr
  -- |
  -- Function application
  --
  | App Expr Expr
  -- |
  -- Variable
  --
  | Var (Qualified Ident)
  -- |
  -- Case expression
  --
  | Case (Array Expr) (Array CaseAlternative)

derive instance eqExpr :: Eq Expr
derive instance ordExpr :: Ord Expr
derive instance genericExpr :: Generic Expr

instance showExpr :: Show Expr where
  show = gShow

instance isForeignExpr :: IsForeign Expr where
  read x = do
    label <- readProp 0 x >>= readString
    readExpr' label x

    where

    readExpr' :: String -> Foreign -> F Expr
    readExpr' "Literal" y = Literal <$> readProp 1 y
    readExpr' "Abs" y = do
      ident <- readProp 1 y
      expr <- readProp 2 y
      Abs <$> readIdent ident <*> read expr
    readExpr' "App" y = do
      expr1 <- readProp 1 y
      expr2 <- readProp 2 y
      App <$> read expr1 <*> read expr2
    readExpr' "Var" y = do
      value <- readProp 1 y
      Var <$> read value
    readExpr' "Case" y = Case <$> readProp 1 y <*> readProp 2 y
    readExpr' label _ = fail $ ForeignError $ "Unknown expression: " <> label

readExprJSON :: String -> F Expr
readExprJSON = parseJSON >=> read

type Guard = Expr

newtype CaseAlternative = CaseAlternative
  { binders :: Array Binder
  --, result :: Either (Array (Tuple Guard Expr)) Expr
  , result :: Expr
  }

derive instance eqCaseAlternative :: Eq CaseAlternative
derive instance ordCaseAlternative :: Ord CaseAlternative
derive instance genericCaseAlternative :: Generic CaseAlternative

instance showCaseAlternative :: Show CaseAlternative where
  show = gShow

instance isForeignCaseAlternative :: IsForeign CaseAlternative where
  read x = do
    binders <- readProp 0 x
    result <- readProp 1 x
    pure $ CaseAlternative { binders, result }

-- |
--  A let or module binding.
--
data Bind = Bind (Array (Tuple Ident Expr))

derive instance eqBind :: Eq Bind
derive instance ordBind :: Ord Bind

instance showBind :: Show Bind where
  show (Bind x) = "(Bind " <> show x <> ")"

instance isForeignBind :: IsForeign Bind where
  read x = do
    pairs <- objectProps x
    bindings <- traverse fromPair pairs
    pure $ Bind bindings

    where

    fromPair
      :: { key :: String, value :: Foreign }
      -> F (Tuple Ident Expr)
    fromPair pair = do
      expr <- read pair.value
      let ident = Ident pair.key
      pure $ Tuple ident expr

readBindJSON :: String -> F Bind
readBindJSON = parseJSON >=> read
