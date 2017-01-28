module CoreFn.Binders where

import Prelude

import Data.Foreign (F, Foreign, ForeignError(..), fail, readString)
import Data.Foreign.Class (class IsForeign, read, readProp)
import Control.Alt ((<|>))
import CoreFn.Literals (Literal)
import CoreFn.Ident (Ident, readIdent)
import CoreFn.Names (Qualified, ProperName)
import Data.Generic (class Generic, gShow)

data Binder
    -- |
    -- Wildcard binder
    --
    = NullBinder
    -- |
    -- A binder which matches a literal value
    --
    | LiteralBinder (Literal Binder)
    -- |
    -- A binder which binds an identifier
    --
    | VarBinder Ident
    -- |
    -- A binder which matches a data constructor
    --
    | ConstructorBinder (Qualified ProperName) (Qualified ProperName) (Array Binder)
    -- -- |
    -- -- A binder which binds its input to an identifier
    -- --
    -- | NamedBinder a Ident (Binder a)

derive instance eqBinder :: Eq Binder
derive instance ordBinder :: Ord Binder
derive instance genericBinder :: Generic Binder

instance showBinder :: Show Binder where
  show = gShow

instance isForeignBinder :: IsForeign Binder where
  read x = nullBinder x <|> nonNullBinder x
  
    where
    nullBinder :: Foreign -> F Binder
    nullBinder x' = do
      label <- readString x'
      if label == "NullBinder"
        then pure NullBinder
        else fail $ ForeignError $ "Unknown binder: " <> label

    nonNullBinder :: Foreign -> F Binder
    nonNullBinder x' = do
      label <- readProp 0 x' >>= readString
      value <- readProp 1 x'
      case label of
        "LiteralBinder" -> LiteralBinder <$> read value
        "VarBinder" -> VarBinder <$> readIdent value
        --"ConstructorBinder" -> do
        --  typeName <- readProp 1 x'
        --  consName <- readProp 2 x'
        --  args <- readProp 3 x'
        --  ConstructorBinder <$> read typeName <*> readProp 2 x' <*> readProp 3 x'
        other -> fail $ ForeignError $ "Unknown binder: " <> other
