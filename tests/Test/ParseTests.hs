{-| Parser tests.

Each test name encodes the LANGUAGE.md section it covers per
section 16.14. The suite is grouped by grammar production.

-}
module Test.ParseTests (suite) where

import qualified Data.List as List
import qualified Data.Text as T
import NriPrelude
import Quone.Parse.Cst
import Quone.Parse.Parser (parseProgram)
import Test.Harness
    ( Suite
    , Test
    , TestResult (Fail, Pass)
    , assert
    , assertLeft
    , assertRight
    , describe
    , test
    , (===)
    )
import qualified Prelude



suite :: Suite
suite =
    describe
        "parser"
        ( programTests
            ++ moduleTests
            ++ declTests
            ++ exprTests
            ++ precedenceTests
            ++ literalTests
            ++ patternTests
            ++ desugarTouchpoints
        )



-- ---------------------------------------------------------------------
-- Programs and modules
-- ---------------------------------------------------------------------


programTests :: [Test]
programTests =
    [ test "parse/empty_program_section_5_1" <|
        Prelude.pure (assertRight (parseProgram ""))
    , test "parse/script_no_module_header_section_4_1" <|
        case parseProgram "x <- 1" of
            Prelude.Right (CProgram {programModule = Nothing, programDecls = [CDValue _]}) ->
                Prelude.pure Pass
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/multiple_top_level_decls_section_4_1" <|
        case parseProgram "x <- 1\ny <- 2" of
            Prelude.Right (CProgram {programDecls = decls}) ->
                Prelude.pure (Prelude.length decls === 2)
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    ]


moduleTests :: [Test]
moduleTests =
    [ test "parse/module_decl_explicit_exports_section_4_2" <|
        case parseProgram "module Stats.Transform exporting (normalize, rmse)" of
            Prelude.Right (CProgram {programModule = Just m}) ->
                Prelude.pure
                    ( assert
                        ( Prelude.length (moduleDeclPath m) Prelude.== 2
                            Prelude.&& isExportNames (moduleDeclExports m)
                        )
                        "expected dotted path with two segments and explicit exports"
                    )
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/module_decl_wildcard_export_section_4_2" <|
        case parseProgram "module Foo exporting (..)" of
            Prelude.Right (CProgram {programModule = Just m}) ->
                Prelude.pure
                    ( case moduleDeclExports m of
                        CExportAll _ -> Pass
                        _ -> Fail "expected CExportAll"
                    )
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    ]


isExportNames :: CExportList -> Prelude.Bool
isExportNames = \case
    CExportNames _ _ -> Prelude.True
    _ -> Prelude.False



-- ---------------------------------------------------------------------
-- Declarations
-- ---------------------------------------------------------------------


declTests :: [Test]
declTests =
    [ test "parse/value_decl_section_5_1" <|
        case parseProgram "x <- 1" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure (lowerNameText (valueDeclName v) === "x")
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/value_decl_with_annotation_section_5_1" <|
        let
            src =
                T.unlines
                    [ "x : Integer"
                    , "x <- 1"
                    ]
        in
        case parseProgram src of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( assert
                        (Prelude.maybe Prelude.False (Prelude.const Prelude.True) (valueDeclAnnotation v))
                        "expected an annotation"
                    )
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/type_decl_single_variant_section_5_1" <|
        case parseProgram "type Bit <- Zero" of
            Prelude.Right (CProgram {programDecls = [CDType d]}) ->
                Prelude.pure (Prelude.length (typeDeclVariants d) === 1)
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/type_decl_multi_variant_section_5_1" <|
        let
            src =
                T.unlines
                    [ "type Maybe a"
                    , "    <- Nothing"
                    , "     | Just a"
                    ]
        in
        case parseProgram src of
            Prelude.Right (CProgram {programDecls = [CDType d]}) ->
                Prelude.pure (Prelude.length (typeDeclVariants d) === 2)
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/type_alias_section_5_1" <|
        let
            src = "type alias Score <- Double"
        in
        case parseProgram src of
            Prelude.Right (CProgram {programDecls = [CDTypeAlias _]}) ->
                Prelude.pure Pass
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/foreign_import_section_4_5" <|
        let
            src = "import readr.read_csv : Character -> dataframe { name : Vector Character }"
        in
        case parseProgram src of
            Prelude.Right (CProgram {programDecls = [CDImport (CForeignImport _ _ _)]}) ->
                Prelude.pure Pass
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/quone_import_single_name_section_4_5" <|
        case parseProgram "import Stats.Transform.normalize" of
            Prelude.Right (CProgram {programDecls = [CDImport (CQuoneImport _ _ (CImportSingle _))]}) ->
                Prelude.pure Pass
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/quone_import_multi_name_section_4_5" <|
        case parseProgram "import Stats.Transform (normalize, rmse)" of
            Prelude.Right (CProgram {programDecls = [CDImport (CQuoneImport _ _ (CImportNames items))]}) ->
                Prelude.pure (Prelude.length items === 2)
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/quone_import_wildcard_section_4_5" <|
        case parseProgram "import Stats.Transform (..)" of
            Prelude.Right (CProgram {programDecls = [CDImport (CQuoneImport _ _ CImportAll)]}) ->
                Prelude.pure Pass
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    ]



-- ---------------------------------------------------------------------
-- Expressions
-- ---------------------------------------------------------------------


exprTests :: [Test]
exprTests =
    [ test "parse/expr_application_curried_section_5_1" <|
        case parseProgram "x <- f 1 2" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( assert
                        (isApp (valueDeclBody v))
                        "expected application"
                    )
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/expr_lambda_section_5_1" <|
        case parseProgram "x <- \\a b -> a + b" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CELambda _ params _ -> Prelude.length params === 2
                        _ -> Fail "expected lambda"
                    )
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/expr_let_section_5_1" <|
        let
            src = "x <- let y <- 1 in y + 2"
        in
        case parseProgram src of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CELet _ bindings _ -> Prelude.length bindings === 1
                        _ -> Fail "expected let"
                    )
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/expr_pipe_section_5_1" <|
        case parseProgram "x <- xs |> f |> g" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( assert
                        (isPipe (valueDeclBody v))
                        "expected pipe expression"
                    )
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/expr_record_literal_section_5_1" <|
        case parseProgram "x <- { name = \"Alice\", score = 92.0 }" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CERecord _ fields -> Prelude.length fields === 2
                        _ -> Fail "expected record literal"
                    )
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/expr_record_update_section_5_1" <|
        case parseProgram "x <- { student | score = 95.0 }" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CERecordUpdate _ _ fields -> Prelude.length fields === 1
                        _ -> Fail (T.pack (Prelude.show (valueDeclBody v)))
                    )
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/expr_dataframe_literal_section_5_1" <|
        case parseProgram "x <- dataframe { name = [\"a\"], score = [1.0] }" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CEDataframe _ fields -> Prelude.length fields === 2
                        _ -> Fail "expected dataframe literal"
                    )
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/expr_vector_literal_section_5_1" <|
        case parseProgram "x <- [1, 2, 3]" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CEVector _ items -> Prelude.length items === 3
                        _ -> Fail "expected vector literal"
                    )
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/expr_field_access_section_5_1" <|
        case parseProgram "x <- row.score" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CEField _ _ _ -> Pass
                        _ -> Fail "expected field access"
                    )
            other ->
                Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/expr_dplyr_verb_section_5_1" <|
        case parseProgram "x <- students |> filter (score > 70.0)" of
            Prelude.Right (CProgram {programDecls = [CDValue _]}) -> Prelude.pure Pass
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    ]


isApp :: CExpr -> Prelude.Bool
isApp (CEApp _ _ _) = Prelude.True
isApp _ = Prelude.False


isPipe :: CExpr -> Prelude.Bool
isPipe (CEPipe _ _ _) = Prelude.True
isPipe _ = Prelude.False



-- ---------------------------------------------------------------------
-- Operator precedence and associativity
-- ---------------------------------------------------------------------


precedenceTests :: [Test]
precedenceTests =
    [ test "parse/precedence_addition_left_assoc_section_5_2" <|
        -- 1 + 2 + 3 should parse as ((1 + 2) + 3)
        case parseProgram "x <- 1 + 2 + 3" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CEBinOp _ COpAdd (CEBinOp _ COpAdd _ _) _ -> Pass
                        other -> Fail ("expected ((1+2)+3); got " ++ T.pack (Prelude.show other))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/precedence_caret_right_assoc_section_5_2" <|
        -- 2 ^ 3 ^ 2 should parse as (2 ^ (3 ^ 2)) = 2 ^ 9 = 512
        case parseProgram "x <- 2 ^ 3 ^ 2" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CEBinOp _ COpExp _ (CEBinOp _ COpExp _ _) -> Pass
                        other -> Fail ("expected (2 ^ (3 ^ 2)); got " ++ T.pack (Prelude.show other))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/precedence_neg_tighter_than_caret_section_5_2" <|
        -- -2 ^ 2 should parse as (-2) ^ 2 = 4 (NOT R's -(2^2) = -4)
        case parseProgram "x <- -2 ^ 2" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CEBinOp _ COpExp (CEUnary _ COpNeg _) _ -> Pass
                        other -> Fail ("expected ((-2) ^ 2); got " ++ T.pack (Prelude.show other))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/precedence_mul_tighter_than_add_section_5_2" <|
        -- 1 + 2 * 3 should parse as 1 + (2 * 3)
        case parseProgram "x <- 1 + 2 * 3" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CEBinOp _ COpAdd _ (CEBinOp _ COpMul _ _) -> Pass
                        other -> Fail ("expected (1 + (2 * 3)); got " ++ T.pack (Prelude.show other))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/precedence_pipe_lowest_section_5_2" <|
        -- xs |> f + 1 should parse as xs |> (f + 1)
        case parseProgram "x <- xs |> f + 1" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CEPipe _ _ (CEBinOp _ COpAdd _ _) -> Pass
                        other -> Fail (T.pack (Prelude.show other))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/precedence_app_tighter_than_mul_section_5_2" <|
        -- f x * 2 should parse as (f x) * 2
        case parseProgram "x <- f x * 2" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CEBinOp _ COpMul (CEApp _ _ _) _ -> Pass
                        other -> Fail (T.pack (Prelude.show other))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    ]



-- ---------------------------------------------------------------------
-- Literals
-- ---------------------------------------------------------------------


literalTests :: [Test]
literalTests =
    [ test "parse/literal_integer_section_5_1" <|
        -- Per LANGUAGE.md section 3.3, an Integer literal MUST carry
        -- the trailing `L` suffix; a bare `42` is a Double.
        case parseProgram "x <- 42L" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CELit _ (CLInt 42) -> Pass
                        _ -> Fail "expected CLInt 42"
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/literal_bare_digits_is_double_section_5_1" <|
        case parseProgram "x <- 42" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CELit _ (CLDouble _) -> Pass
                        _ -> Fail "expected CLDouble for bare 42"
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/literal_double_section_5_1" <|
        case parseProgram "x <- 3.14" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CELit _ (CLDouble _) -> Pass
                        _ -> Fail "expected CLDouble"
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/literal_string_section_5_1" <|
        case parseProgram "x <- \"hello\"" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CELit _ (CLChar "hello") -> Pass
                        _ -> Fail "expected CLChar"
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    ]



-- ---------------------------------------------------------------------
-- Patterns
-- ---------------------------------------------------------------------


patternTests :: [Test]
patternTests =
    [ test "parse/pattern_wildcard_section_5_4" <|
        case parseProgram "x <- case y of\n    _ -> 1" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CECase _ _ [CCaseArm {caseArmPattern = CPWildcard _}] -> Pass
                        other -> Fail (T.pack (Prelude.show other))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/pattern_constructor_section_5_4" <|
        case parseProgram "x <- case y of\n    Just n -> n" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CECase _ _ [CCaseArm {caseArmPattern = CPCon _ _ [_]}] -> Pass
                        other -> Fail (T.pack (Prelude.show other))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/pattern_record_short_section_5_4" <|
        case parseProgram "x <- case y of\n    { name, score } -> name" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CECase _ _ [CCaseArm {caseArmPattern = CPRecord _ fields}] ->
                            Prelude.length fields === 2
                        other -> Fail (T.pack (Prelude.show other))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "parse/pattern_record_full_section_5_4" <|
        case parseProgram "x <- case y of\n    { name = n } -> n" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CECase _ _ [CCaseArm {caseArmPattern = CPRecord _ [CRpfFull _ _ _]}] -> Pass
                        other -> Fail (T.pack (Prelude.show other))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    ]



-- ---------------------------------------------------------------------
-- Surface forms that the desugar pass will handle later
-- ---------------------------------------------------------------------


desugarTouchpoints :: [Test]
desugarTouchpoints =
    [ test "parse/if_kept_in_cst_section_5_3" <|
        -- The CST keeps `if`; the desugar pass converts it to `case`.
        -- Test 16.5's "no EIf in AST" is a desugar-stage concern.
        case parseProgram "x <- if c then 1 else 2" of
            Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                Prelude.pure
                    ( case valueDeclBody v of
                        CEIf _ _ _ _ -> Pass
                        other -> Fail ("expected CEIf in CST; got " ++ T.pack (Prelude.show other))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    ]
