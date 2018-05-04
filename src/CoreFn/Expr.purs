-- |
-- The core functional representation
--
module CoreFn.Expr
  ( Bind(..)
  , CaseAlternative(..)
  , Expr(..)
  , Guard
  ) where

import Prelude

import CoreFn.Binders (Binder)
import CoreFn.Ident (Ident)
import CoreFn.Literal (Literal)
import CoreFn.Names (ProperName, Qualified)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..), either)
import Data.Profunctor.Strong ((***))
import Data.Traversable (intercalate)
import Data.Tuple (Tuple)

-- |
-- Data type for expressions and terms
--
data Expr a
  -- |
  -- A literal value
  --
  = Literal a (Literal (Expr a))
  -- |
  -- A data constructor (type name, constructor name, field names)
  --
  | Constructor a ProperName ProperName (Array Ident)
  -- |
  -- A record property accessor
  --
  | Accessor a String (Expr a) -- PSString
  -- |
  -- Partial record update
  --
  | ObjectUpdate a (Expr a) (Array (Tuple String (Expr a))) -- PSString
  -- |
  -- Function introduction
  --
  | Abs a Ident (Expr a)
  -- |
  -- Function application
  --
  | App a (Expr a) (Expr a)
  -- |
  -- Variable
  --
  | Var a (Qualified Ident)
  -- |
  -- A case expression
  --
  | Case a (Array (Expr a)) (Array (CaseAlternative a))
  -- |
  -- A let binding
  --
  | Let a (Array (Bind a)) (Expr a)

derive instance eqExpr :: Eq a => Eq (Expr a)
derive instance functorExpr :: Functor Expr
derive instance ordExpr :: Ord a => Ord (Expr a)

instance showExpr :: Show a => Show (Expr a) where
  show (Literal a l) =
    "(Literal " <>
      intercalate " " [ show a, show l ] <>
    ")"
  show (Constructor a t c fs) =
    "(Constructor " <>
      intercalate " " [ show a, show t, show c, show fs] <>
    ")"
  show (Accessor a s e) =
    "(Accessor " <>
      intercalate " " [ show a, show s, show e ] <>
    ")"
  show (ObjectUpdate a e fs) =
    "(ObjectUpdate " <>
      intercalate " " [ show a, show e, show fs ] <>
    ")"
  show (Abs a i e) =
    "(Abs " <>
      intercalate " " [ show a, show i, show e ] <>
    ")"
  show (App a e1 e2) =
    "(App " <>
      intercalate " " [ show a, show e1, show e2 ] <>
    ")"
  show (Var a q) =
    "(Var " <>
      intercalate " " [ show a, show q ] <>
    ")"
  show (Case a es cs) =
    "(Case " <>
      intercalate " " [ show a, show es, show cs ] <>
    ")"
  show (Let a bs e) =
    "(Let " <>
      intercalate " " [ show a, show bs, show e ] <>
    ")"


-- |
--  A let or module binding.
--
data Bind a
  = NonRec a Ident (Expr a)
  | Rec (Array (Tuple (Tuple a Ident) (Expr a)))

derive instance eqBind :: Eq a => Eq (Bind a)
derive instance ordBind :: Ord a => Ord (Bind a)

instance functorBindRec :: Functor Bind where
  map f (NonRec a i e) = NonRec (f a) i (map f e)
  map f (Rec ts) = Rec $ map (bimap (lmap f) (map f)) ts

instance showBind :: Show a => Show (Bind a) where
  show (NonRec a i e) =
    "(NonRec " <>
      intercalate " " [ show a, show i, show e ] <>
    ")"
  show (Rec b) = "(Rec " <> show b <> ")"


-- |
-- A guard is just a boolean-valued expression that appears alongside a set of binders
--
type Guard a = Expr a


-- |
-- An alternative in a case statement
--
data CaseAlternative a = CaseAlternative
  {
    -- |
    -- A collection of binders with which to match the inputs
    caseAlternativeBinders :: (Array (Binder a))
    -- |
    -- The result expression or a collect of guarded expressions
  , caseAlternativeResult :: (Either (Array (Tuple (Guard a) (Expr a))) (Expr a))
  }

derive instance eqCaseAlternative :: Eq a => Eq (CaseAlternative a)
derive instance ordCaseAlternative :: Ord a => Ord (CaseAlternative a)

instance functorCaseAlternative :: Functor CaseAlternative where
  map f (CaseAlternative { caseAlternativeBinders, caseAlternativeResult }) = CaseAlternative
    { caseAlternativeBinders: (map (map f) caseAlternativeBinders)
    , caseAlternativeResult: (either (Left <<< map (map f *** map f)) (Right <<< map f) caseAlternativeResult)
    }

instance showCaseAlternative :: Show a => Show (CaseAlternative a) where
  show (CaseAlternative { caseAlternativeBinders, caseAlternativeResult }) =
    "(CaseAlternative " <>
      "{ caseAlternativeBinders: " <> show caseAlternativeBinders <>
      ", caseAlternativeResult: " <> show caseAlternativeResult <> " " <>
      "}" <>
    ")"
