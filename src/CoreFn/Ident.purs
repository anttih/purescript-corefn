-- |
-- Names for value identifiers
--
module CoreFn.Ident
  ( Ident(..)
  , readIdent
  , readIdentJSON
  ) where

import Prelude
import Data.Foreign (F, Foreign, parseJSON, readString)
import Data.Generic (gShow, class Generic)
import Data.Newtype (class Newtype)

-- |
-- An alphanumeric identifier
--
newtype Ident = Ident String

derive instance eqIdent :: Eq Ident
derive instance genericIdent :: Generic Ident
derive instance ordIdent :: Ord Ident
derive instance newtypeIdent :: Newtype Ident _

instance showIdent :: Show Ident where
  show = gShow

readIdent :: Foreign -> F Ident
readIdent x = Ident <$> readString x

readIdentJSON :: String -> F Ident
readIdentJSON = parseJSON >=> readIdent
