module CoreFn.Literals
  ( Literal(..)
  , readLiteralJSON
  ) where

import Prelude

import Data.Foreign (F, Foreign, ForeignError(..), fail, parseJSON)
import Data.Foreign.Class (class IsForeign, read, readProp)
import Data.Foreign.Index (prop)
import Data.Foreign.Keys as K
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Generic (class Generic, gShow)
import Data.Traversable (sequence)

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
derive instance genericLiteral :: Generic a => Generic (Literal a)

instance showLiteral :: Generic a => Show (Literal a) where
  show = gShow

instance isForeignLiteral :: IsForeign a => IsForeign (Literal a) where
  read x = do
    label <- readProp 0 x
    readLiteral' label x

    where

    readPair :: Foreign -> String -> F (Tuple String a)
    readPair obj key = Tuple key <$> (prop key obj >>= read)

    readPairs :: Foreign -> Array String -> F (Array (Tuple String a))
    readPairs obj = sequence <<< (map <<< readPair) obj

    readLiteral' :: String -> Foreign -> F (Literal a)
    readLiteral' "IntLiteral" v = NumericLiteral <$> Left <$> readProp 1 v
    readLiteral' "NumberLiteral" v = NumericLiteral <$> Right <$> readProp 1 v
    readLiteral' "StringLiteral" v = StringLiteral <$> readProp 1 v
    readLiteral' "CharLiteral" v = CharLiteral <$> readProp 1 v
    readLiteral' "BooleanLiteral" v = BooleanLiteral <$> readProp 1 v
    readLiteral' "ArrayLiteral" v = ArrayLiteral <$> readProp 1 v
    readLiteral' "ObjectLiteral" v = do
      obj <- readProp 1 v
      keys <- K.keys obj
      ObjectLiteral <$> readPairs obj keys
    readLiteral' label _ = fail $ ForeignError $ "Unknown literal: " <> label

readLiteralJSON :: forall a. IsForeign a => String -> F (Literal a)
readLiteralJSON = parseJSON >=> read
