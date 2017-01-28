module Test.CoreFn.Expr
  ( testBindings
  , testExpr
  , testLiterals
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import CoreFn.Expr (Bind(..), Expr(..), CaseAlternative(..), readBindJSON, readExprJSON)
import CoreFn.Ident (Ident(..))
import CoreFn.Names (ModuleName(..), Qualified(..))
import CoreFn.Binders (Binder(..))
import CoreFn.Literals (Literal(..), readLiteralJSON)
import Data.Foreign (F, ForeignError(..))
import Data.Either (Either(..))
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Test.Util (assertEqual, expectFailure, expectSuccess)

readLiteralJsonExpr :: String -> F (Literal Expr)
readLiteralJsonExpr = readLiteralJSON

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

    expectSuccess description (readLiteralJsonExpr json) \x ->
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

    expectSuccess description (readLiteralJsonExpr json) \x ->
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

    expectSuccess description (readLiteralJsonExpr json) \x ->
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

    expectSuccess description (readLiteralJsonExpr json) \x ->
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

    expectSuccess description (readLiteralJsonExpr json) \x ->
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

    expectSuccess description (readLiteralJsonExpr json) \x ->
      assertEqual x $ ArrayLiteral
        [ Literal (StringLiteral "Hello world!")
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

    expectSuccess description (readLiteralJsonExpr json) \x ->
      assertEqual x $ ObjectLiteral
        [ Tuple "hello" (Literal (StringLiteral "world!"))
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

    expectFailure description (readLiteralJsonExpr json) \x ->
      assertEqual x (singleton (ForeignError "Unknown literal: SomeLiteral"))

testExpr :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
testExpr = do
  log ""
  log "Test Expr"

  testLiteralExpr
  testAbsExpr
  testAppExpr
  testVarExpr
  testUnknownExpr
  testCaseExpr
  testCaseNullBinder
  testCaseVarBinder

  where

  -- |
  -- Literal
  --
  testLiteralExpr = do
    let description = "Literal from JSON results in success"

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
      assertEqual x (Literal (StringLiteral "Hello world!"))

  -- |
  -- Abs
  --
  testAbsExpr = do
    let description = "Abs from JSON results in success"

    let json = """
      [
        "Abs",
        "x",
        [
          "Var",
          "x"
        ]
      ]
    """

    expectSuccess description (readExprJSON json) \x -> do
      let ident = Ident "x"
      let qualified = Qualified Nothing (Ident "x")
      let var = Var qualified
      assertEqual x (Abs ident var)

  -- |
  -- App
  --
  testAppExpr = do
    let description = "App from JSON results in success"

    let json = """
      [
        "App",
        [
          "Var",
          "Control.Monad.Eff.Console.log"
        ],
        [
          "Literal",
          [
            "StringLiteral",
            "Hello world!"
          ]
        ]
      ]
    """

    expectSuccess description (readExprJSON json) \x -> do
      let moduleName = Just (ModuleName "Control.Monad.Eff.Console")
      let qualified = Qualified moduleName (Ident "log")
      let var = Var qualified
      let literal = Literal (StringLiteral "Hello world!")
      assertEqual x (App var literal)

  -- |
  -- Var
  --
  testVarExpr = do
    let description = "Var from JSON results in success"

    let json = """
      [
        "Var",
        "Control.Monad.Eff.Console.log"
      ]
    """

    expectSuccess description (readExprJSON json) \x -> do
      let moduleName = Just (ModuleName "Control.Monad.Eff.Console")
      let qualified = Qualified moduleName (Ident "log")
      assertEqual x (Var qualified)

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

  
  testCaseNullBinder = do
    let description = "Case expression with null binder"

    let json = """
      [
        "Case",
        [["Var","b"]],
        [
          [
            ["NullBinder"],
            ["Literal",["IntLiteral",1]]
          ]
        ]
      ]
    """

    expectSuccess description (readExprJSON json)	\x -> do
      let var = Var (Qualified Nothing (Ident "b"))
      let alt = CaseAlternative { binders: [NullBinder], result: (Literal (NumericLiteral (Left 1))) }
      --let b1 = CaseAlternative { binders: [litBinderT], result: Right (Literal (NumericLiteral (Left 1))) }
      --let b2 = CaseAlternative { binders: [litBinderF], result: Right (Literal (NumericLiteral (Left 2))) }
      assertEqual x (Case [var] [alt])

  testCaseExpr = do
    let description = "Case expression with literal binders"

    let json = """
      [
        "Case",
        [
          ["Var","b"]],
          [
            [
              [["LiteralBinder",["BooleanLiteral",true]]],
              ["Literal",["IntLiteral",1]]
            ],
            [
              [["LiteralBinder",["BooleanLiteral",false]]],
              ["Literal",["IntLiteral",2]]
            ]
          ]
      ]
    """

    expectSuccess description (readExprJSON json)	\x -> do
      let var = Var (Qualified Nothing (Ident "b"))
      let litBinderT = LiteralBinder (BooleanLiteral true)
      let litBinderF = LiteralBinder (BooleanLiteral false)
      let b1 = CaseAlternative { binders: [litBinderT], result: (Literal (NumericLiteral (Left 1))) }
      let b2 = CaseAlternative { binders: [litBinderF], result: (Literal (NumericLiteral (Left 2))) }
      assertEqual x (Case [var] [b1, b2])

  testCaseVarBinder = do
    let description = "Case expression with var binders"

    let json = """
      [
        "Case",
        [["Var", "v"]],
        [
          [
            [["LiteralBinder", ["IntLiteral", 1]]],
            ["Literal", ["IntLiteral", 1]]
          ],
          [
            [["VarBinder", "i"]],
            ["Var", "i"]
          ]
        ]
      ]
    """

    expectSuccess description (readExprJSON json)	\x -> do
      let var = Var (Qualified Nothing (Ident "i"))
      let litBinder = LiteralBinder (NumericLiteral (Left 1))
      let varBinder = VarBinder (Ident "i")
      let b1 = CaseAlternative { binders: [litBinder], result: (Literal (NumericLiteral (Left 1))) }
      let b2 = CaseAlternative { binders: [varBinder], result: var }
      assertEqual x (Case [Var (Qualified Nothing (Ident "v"))] [b1, b2])

  testCaseConstructorBinder = do
    let description = "Case expression with constructor binder"

    let json = """
        [
          "Case",
          [["Var", "v"]],
          [
              [
                  [
                    ["ConstructorBinder", "Module.Foo", "Module.Bar", []]
                  ],
                  ["Literal", ["IntLiteral", 1]]
              ],
              [
                  [
                      ["ConstructorBinder", "Module.Foo", "Module.Baz", []]
                  ],
                  ["Literal", ["IntLiteral", 2]]
              ]
          ]
      ]
    """

    expectSuccess description (readExprJSON json)	\x -> do
      let var = Var (Qualified Nothing (Ident "i"))
      let litBinder = LiteralBinder (NumericLiteral (Left 1))
      let varBinder = VarBinder (Ident "i")
      let b1 = CaseAlternative { binders: [litBinder], result: (Literal (NumericLiteral (Left 1))) }
      let b2 = CaseAlternative { binders: [varBinder], result: var }
      assertEqual x (Case [Var (Qualified Nothing (Ident "v"))] [b1, b2])

  --testCaseExpr = do
  --  let description = "Case expression with literal binders"

  --  let json = """
  --    [
  --      "Case",
  --      [["Var","v"],["Var","v1"]],
  --      [[[["LiteralBinder",["BooleanLiteral",true]],["VarBinder","i"]],[[["App",["App",["App",["Var","Data.Eq.eq"],["Var","Data.Eq.eqInt"]],["Var","i"]],["Literal",["IntLiteral",0]]],["Literal",["IntLiteral",1]]]]]
  --    ]
  --    ]
  --  """

  --  expectSuccess description (readExprJSON json)	\x -> do
  --    let var = Var (Qualified Nothing (Ident "b"))
  --    let litBinderT = LiteralBinder (BooleanLiteral true)
  --    let litBinderF = LiteralBinder (BooleanLiteral false)
  --    let b1 = CaseAlternative { binders: [litBinderT], result: (Literal (NumericLiteral (Left 1))) }
  --    let b2 = CaseAlternative { binders: [litBinderF], result: (Literal (NumericLiteral (Left 2))) }
  --    --let b1 = CaseAlternative { binders: [litBinderT], result: Right (Literal (NumericLiteral (Left 1))) }
  --    --let b2 = CaseAlternative { binders: [litBinderF], result: Right (Literal (NumericLiteral (Left 2))) }
  --    assertEqual x (Case [var] [b1, b2])


testBindings :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
testBindings = do
  log ""
  log "Test Bind"

  testNoBindings
  testNonRecBind
  testRecBind

  where

  testNoBindings = do
    let description = "No bindings from JSON results in success"

    let json = """
      {}
    """

    expectSuccess description (readBindJSON json) \x ->
      assertEqual x (Bind [])

  -- |
  -- Non-recursive binding
  --
  testNonRecBind = do
    let description = "Non-recursive binding from JSON result in success"

    let json = """
      {
        "main": [
          "App",
          [
            "Var",
            "Control.Monad.Eff.Console.log"
          ],
          [
            "Literal",
            [
              "StringLiteral",
              "Hello world!"
            ]
          ]
        ]
      }
    """

    expectSuccess description (readBindJSON json) \x -> do
      let ident = Ident "main"
      let moduleName = Just (ModuleName "Control.Monad.Eff.Console")
      let qualified = Qualified moduleName (Ident "log")
      let var = Var qualified
      let literal = Literal (StringLiteral "Hello world!")
      let app = App var literal
      let binding = Tuple ident app
      assertEqual x (Bind [binding])

  -- |
  -- Mutually recursive bindings
  --
  testRecBind = do
    let description = "Mutually recursive bindings from JSON result in success"

    let json = """
      {
        "f": [
          "Abs",
          "x",
          [
            "App",
            [
              "Var",
              "Example.g"
            ],
            [
              "Var",
              "x"
            ]
          ]
        ],
        "g": [
          "Abs",
          "x",
          [
            "App",
            [
              "Var",
              "Example.f"
            ],
            [
              "Var",
              "x"
            ]
          ]
        ]
      }
    """

    expectSuccess description (readBindJSON json) \x -> do
      let fIdent = Ident "f"
      let fModuleName = Just (ModuleName "Example")
      let fQualified = Qualified fModuleName (Ident "g")
      let fAppVar1 = Var fQualified
      let fAppVar2 = Var (Qualified Nothing (Ident "x"))
      let fApp = App fAppVar1 fAppVar2
      let fAbs = Abs  (Ident "x") fApp
      let fBinding = Tuple fIdent fAbs

      let gIdent = Ident "g"
      let gModuleName = Just (ModuleName "Example")
      let gQualified = Qualified gModuleName (Ident "f")
      let gAppVar1 = Var gQualified
      let gAppVar2 = Var (Qualified Nothing (Ident "x"))
      let gApp = App gAppVar1 gAppVar2
      let gAbs = Abs (Ident "x") gApp
      let gBinding = Tuple gIdent gAbs

      assertEqual x (Bind [fBinding, gBinding])
