module Test.CoreFn.Expr
  ( testExpr
  , testLiterals
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import CoreFn.Expr (Expr(..), Literal(..), readExprJSON, readLiteralJSON)
import Data.Either (Either(..))
import Data.Foreign (ForeignError(..), toForeign)
import Data.List.NonEmpty (singleton)
import Data.Tuple (Tuple(..))
import Test.Util (assertEqual, expectFailure, expectSuccess)

testLiterals :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
testLiterals = do
  log ""
  log "Test Literals"

  testNumericIntLiteral
  testNumericNumberLiteral
  testStringLiteral
  testCharLiteral
  testBooleanLiteral
  testArrayLiteral
  testObjectLiteral
  testUnknownLiteral

  where

  -- |
  -- NumericLiteral (Int)
  --
  testNumericIntLiteral = do
    let description = "NumericLiteral (Int) from JSON results in success"

    let json = """
      [
        "IntLiteral",
        42
      ]
    """

    expectSuccess description (readLiteralJSON json) \x ->
      assertEqual x (NumericLiteral (Left 42))

  -- |
  -- NumericLiteral (Number)
  --
  testNumericNumberLiteral = do
    let description = "NumericLiteral (Number) from JSON results in success"

    let json = """
      [
        "NumberLiteral",
        3.14
      ]
    """

    expectSuccess description (readLiteralJSON json) \x ->
      assertEqual x (NumericLiteral (Right 3.14))

  -- |
  -- StringLiteral
  --
  testStringLiteral = do
    let description = "StringLiteral from JSON results in success"

    let json = """
      [
        "StringLiteral",
        "Hello World!"
      ]
    """

    expectSuccess description (readLiteralJSON json) \x ->
      assertEqual x (StringLiteral "Hello World!")

  -- |
  -- CharLiteral
  --
  testCharLiteral = do
    let description = "CharLiteral from JSON results in success"

    let json = """
      [
        "CharLiteral",
        "a"
      ]
    """

    expectSuccess description (readLiteralJSON json) \x ->
      assertEqual x (CharLiteral 'a')

  -- |
  -- BooleanLiteral
  --
  testBooleanLiteral = do
    let description = "BooleanLiteral from JSON results in success"

    let json = """
      [
        "BooleanLiteral",
        true
      ]
    """

    expectSuccess description (readLiteralJSON json) \x ->
      assertEqual x (BooleanLiteral true)

  -- |
  -- ArrayLiteral
  --
  testArrayLiteral = do
    let description = "ArrayLiteral from JSON results in success"

    let json = """
      [
        "ArrayLiteral",
        [
          [
            "Literal",
            [
              "StringLiteral",
              "Hello world!"
            ]
          ]
        ]
      ]
    """

    expectSuccess description (readLiteralJSON json) \x ->
      assertEqual x $ ArrayLiteral
        [ Literal (toForeign unit) (StringLiteral "Hello world!")
        ]

  -- |
  -- ObjectLiteral
  --
  testObjectLiteral = do
    let description = "ObjectLiteral from JSON results in success"

    let json = """
      [
        "ObjectLiteral",
        {
          "hello": [
            "Literal",
            [
              "StringLiteral",
              "world!"
            ]
          ]
        }
      ]
    """

    expectSuccess description (readLiteralJSON json) \x ->
      assertEqual x $ ObjectLiteral
        [ Tuple "hello" (Literal (toForeign unit) (StringLiteral "world!"))
        ]

  -- |
  -- Unknown
  --
  testUnknownLiteral = do
    let description = "Unknown literal from JSON results in error"

    let json = """
      [
        "SomeLiteral",
        "some value"
      ]
    """

    expectFailure description (readLiteralJSON json) \x ->
      assertEqual x (singleton (ForeignError "Unknown literal: SomeLiteral"))

testExpr :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
testExpr = do
  log ""
  log "Test Expr"

  testLiteralExpr
  testUnknownExpr

  where

  -- |
  -- Literal
  --
  testLiteralExpr = do
    let description = "StringLiteral from JSON results in success"

    let json = """
      [
        "Literal",
        [
          "StringLiteral",
          "Hello world!"
        ]
      ]
    """

    expectSuccess description (readExprJSON json) \x ->
      assertEqual x (Literal (toForeign unit) (StringLiteral "Hello world!"))

  -- |
  -- Unknown
  --
  testUnknownExpr = do
    let description = "Unknown expression from JSON results in error"

    let json = """
      [
        "SomeExpression",
        [
          "Literal",
          [
            "StringLiteral",
            "Hello world!"
          ]
        ]
      ]
    """

    expectFailure description (readExprJSON json) \x ->
      assertEqual x (singleton (ForeignError "Unknown expression: SomeExpression"))