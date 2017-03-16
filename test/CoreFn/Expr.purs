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
import CoreFn.Names (ModuleName(..), Qualified(..), ProperName(..))
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
  testCaseConstructorBinder
  testCaseConstructorBinderArgs
  testCaseNamedBinder
  testCaseWithGuard
  testCaseWithMultipleGuards

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
      let alt = CaseAlternative { binders: [NullBinder]
                                , result: Right (Literal (NumericLiteral (Left 1))) }
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
      let b1 = CaseAlternative { binders: [litBinderT]
                               , result: Right (Literal (NumericLiteral (Left 1))) }
      let b2 = CaseAlternative { binders: [litBinderF]
                               , result: Right (Literal (NumericLiteral (Left 2))) }
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
      let b1 = CaseAlternative { binders: [litBinder]
                               , result: Right (Literal (NumericLiteral (Left 1))) }
      let b2 = CaseAlternative { binders: [varBinder], result: Right var }
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
      let moduleName = Just (ModuleName "Module")
      let typeName = (Qualified moduleName (ProperName "Foo"))
      let constBinder1 = ConstructorBinder typeName (Qualified moduleName (ProperName "Bar")) []
      let constBinder2 = ConstructorBinder typeName (Qualified moduleName (ProperName "Baz")) []
      let b1 = CaseAlternative { binders: [constBinder1]
                               , result: Right (Literal (NumericLiteral (Left 1))) }
      let b2 = CaseAlternative { binders: [constBinder2]
                               , result: Right (Literal (NumericLiteral (Left 2))) }
      assertEqual x (Case [Var (Qualified Nothing (Ident "v"))] [b1, b2])

  testCaseConstructorBinderArgs = do
    let description = "Case expression with constructor binder arguments"

    let json = """
      [
        "Case",
        [["Var","v"]],
        [
          [
            [
              ["ConstructorBinder","Module.Foo","Module.Two",[["VarBinder","x"],["VarBinder","y"]]]
            ],
            ["Literal", ["IntLiteral",1]]
          ]
        ]
      ]
    """

    expectSuccess description (readExprJSON json)	\x -> do
      let moduleName = Just (ModuleName "Module")
      let typeName = (Qualified moduleName (ProperName "Foo"))
      let args = [VarBinder (Ident "x"), VarBinder (Ident "y")]
      let constBinder1 = ConstructorBinder typeName (Qualified moduleName (ProperName "Two")) args
      let b = CaseAlternative { binders: [constBinder1]
                              , result: Right (Literal (NumericLiteral (Left 1))) }
      assertEqual x (Case [Var (Qualified Nothing (Ident "v"))] [b])

  testCaseNamedBinder = do
    let description = "Case expression with a named binder"

    let json = """
      [
        "Case",
        [["Var","v"]],
        [
          [
            [
              [
                "NamedBinder",
                "foo",
                ["ConstructorBinder","Module.Sum","Module.One",[["VarBinder","i"]]]
              ]
            ],
            ["Var","i"]
          ]
        ]
      ]
    """

    expectSuccess description (readExprJSON json)	\x -> do
      let moduleName = Just (ModuleName "Module")
      let typeName = (Qualified moduleName (ProperName "Sum"))
      let args = [VarBinder (Ident "i")]
      let constBinder = ConstructorBinder typeName (Qualified moduleName (ProperName "One")) args
      let namedBinder = NamedBinder (Ident "foo") constBinder
      let b = CaseAlternative { binders: [namedBinder]
                              , result: Right (Var (Qualified Nothing (Ident "i"))) }
      assertEqual x (Case [Var (Qualified Nothing (Ident "v"))] [b])

  testCaseWithGuard = do
    let description = "Case expression with a guard"

    let json = """
      [
        "Case",
        [["Var","v"]],
        [
          [
            ["NullBinder"],
            [
              [
                ["Literal",["BooleanLiteral",true]],
                ["Literal",["IntLiteral",1]]
              ]
            ]
          ]
        ]
      ]
    """

    expectSuccess description (readExprJSON json)	\x -> do
      let guards = [Tuple (Literal (BooleanLiteral true)) (Literal (NumericLiteral (Left 1)))]
      let b = CaseAlternative { binders: [NullBinder]
                              , result: Left guards }
      assertEqual x (Case [Var (Qualified Nothing (Ident "v"))] [b])

  testCaseWithMultipleGuards = do
    let description = "Case expression with multiple guards"

    let json = """
      [
        "Case",
        [["Var","i"]],
        [
          [
            [["VarBinder","i1"]],
            [
              [
                ["Literal",["BooleanLiteral",true]],
                ["Literal",["IntLiteral",1]]
              ],
              [
                ["Literal",["BooleanLiteral",false]],
                ["Literal",["IntLiteral",2]]
              ],
              [
                ["Var","Data.Boolean.otherwise"],
                ["Literal",["IntLiteral",3]]
              ]
            ]
          ]
        ]
      ]
    """

    expectSuccess description (readExprJSON json)	\x -> do
      let moduleName = Just (ModuleName "Data.Boolean")
      let guards = [ Tuple (Literal (BooleanLiteral true)) (Literal (NumericLiteral (Left 1)))
                   , Tuple (Literal (BooleanLiteral false)) (Literal (NumericLiteral (Left 2)))
                   , Tuple (Var (Qualified moduleName (Ident "otherwise"))) (Literal (NumericLiteral (Left 3)))
                   ]
      let b = CaseAlternative { binders: [VarBinder (Ident "i1")]
                              , result: Left guards }
      assertEqual x (Case [Var (Qualified Nothing (Ident "i"))] [b])

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
