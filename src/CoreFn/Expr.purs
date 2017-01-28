-- |
-- The core functional representation
--
module CoreFn.Expr
  ( Bind(..)
  , Expr(..)
  , Literal(..)
  , readBind
  , readBindJSON
  , readExprJSON
  , readLiteral
  , readLiteralJSON
  ) where

import Prelude
import Data.Foreign.Keys as K
import CoreFn.Ident (Ident(..), readIdent)
import CoreFn.Names (Qualified, readQualified)
import CoreFn.Util (objectProps)
import Data.Either (Either(..), either)
import Data.Foreign (F, Foreign, ForeignError(..), fail, parseJSON, readArray, readBoolean, readChar, readInt, readNumber, readString)
import Data.Foreign.Class (class IsForeign, read, readProp)
import Data.Foreign.Index (prop)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))

-- |
-- Data type for literal values. Parameterised so it can be used for Exprs and
-- Binders.
--
data Literal a
  -- |
  -- A numeric literal
  --
  = NumericLiteral (Either Int Number)
  -- |
  -- A string literal
  --
  | StringLiteral String
  -- |
  -- A character literal
  --
  | CharLiteral Char
  -- |
  -- A boolean literal
  --
  | BooleanLiteral Boolean
  -- |
  -- An array literal
  --
  | ArrayLiteral (Array a)
  -- |
  -- An object literal
  --
  | ObjectLiteral (Array (Tuple String a))

derive instance eqLiteral :: Eq a => Eq (Literal a)
derive instance ordLiteral :: Ord a => Ord (Literal a)

instance showLiteral :: Show a => Show (Literal a) where
  show (NumericLiteral e) = "(NumericLiteral " <> either show show e <> ")"
  show (StringLiteral s) = "(StringLiteral " <> show s <> ")"
  show (CharLiteral c) = "(CharLiteral " <> show c <> ")"
  show (BooleanLiteral b) = "(BooleanLiteral " <> show b <> ")"
  show (ArrayLiteral a) = "(ArrayLiteral " <> show a <> ")"
  show (ObjectLiteral o) = "(ObjectLiteral" <> show o <> ")"

readLiteral :: Foreign -> F (Literal Expr)
readLiteral x = do
  label <- readProp 0 x >>= readString
  readLiteral' label x

  where

  readValues :: Array Foreign -> F (Array Expr)
  readValues = traverse read

  readPair :: Foreign -> String -> F (Tuple String Expr)
  readPair obj key = Tuple key <$> (prop key obj >>= read)

  readPairs :: Foreign -> Array String -> F (Array (Tuple String Expr))
  readPairs obj = sequence <<< (map <<< readPair) obj

  readLiteral' :: String -> Foreign -> F (Literal Expr)
  readLiteral' "IntLiteral" v = do
    value <- readProp 1 v
    NumericLiteral <$> Left <$> readInt value
  readLiteral' "NumberLiteral" v = do
    value <- readProp 1 v
    NumericLiteral <$> Right <$> readNumber value
  readLiteral' "StringLiteral" v = do
    value <- readProp 1 v
    StringLiteral <$> readString value
  readLiteral' "CharLiteral" v = do
    value <- readProp 1 v
    CharLiteral <$> readChar value
  readLiteral' "BooleanLiteral" v = do
    value <- readProp 1 v
    BooleanLiteral <$> readBoolean value
  readLiteral' "ArrayLiteral" v = do
    array <- readProp 1 v >>= readArray
    ArrayLiteral <$> readValues array
  readLiteral' "ObjectLiteral" v = do
    obj <- readProp 1 v
    keys <- K.keys obj
    ObjectLiteral <$> readPairs obj keys
  readLiteral' label _ = fail $ ForeignError $ "Unknown literal: " <> label

readLiteralJSON :: String -> F (Literal Expr)
readLiteralJSON = parseJSON >=> readLiteral

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

derive instance eqExpr :: Eq Expr
derive instance ordExpr :: Ord Expr

instance showExpr :: Show Expr where
  show (Literal x) = "(Literal " <> show x <> ")"
  show (Abs x y) = "(Abs " <> show x <> " " <> show y <> ")"
  show (App x y) = "(App " <> show x <> " " <> show y <> ")"
  show (Var x) = "(Var " <> show x <> ")"

instance isForeignExpr :: IsForeign Expr where
  read x = do
    label <- readProp 0 x >>= readString
    readExpr' label x

    where

    readExpr' :: String -> Foreign -> F Expr
    readExpr' "Literal" y = do
      value <- readProp 1 y
      Literal <$> readLiteral value
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
      Var <$> readQualified Ident value
    readExpr' label _ = fail $ ForeignError $ "Unknown expression: " <> label

readExprJSON :: String -> F Expr
readExprJSON = parseJSON >=> read

-- |
--  A let or module binding.
--
data Bind a = Bind (Array (Tuple (Tuple a Ident) Expr))

derive instance eqBind :: Eq a => Eq (Bind a)
derive instance ordBind :: Ord a => Ord (Bind a)

instance showBind :: Show a => Show (Bind a) where
  show (Bind x) = "(Bind " <> show x <> ")"

readBind :: Foreign -> F (Bind Unit)
readBind x = do
  pairs <- objectProps x
  bindings <- traverse fromPair pairs
  pure $ Bind bindings

  where

  fromPair
    :: { key :: String, value :: Foreign }
    -> F (Tuple (Tuple Unit Ident) Expr)
  fromPair pair = do
    expr <- read pair.value
    let ident = Ident pair.key
    pure $ Tuple (Tuple unit ident) expr

readBindJSON :: String -> F (Bind Unit)
readBindJSON = parseJSON >=> readBind
