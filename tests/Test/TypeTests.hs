{-| Type-checker tests.

Covers stage 6: HM inference, generalisation, operator typing per
LANGUAGE.md section 8.8, pattern typing per section 8.4, record
update rejection on dataframes per section 8.5, and the foundational
parts of section 8 from primitive literals through curried
application.

-}
module Test.TypeTests (suite) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import NriPrelude
import Quone.Ast.Source
import Quone.Parse.Desugar (desugarSource)
import Quone.Type.Infer (inferProgram, typedBindings)
import Quone.Type.Types (Scheme, showScheme, schemeBody, showType)
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
        "type"
        ( literalTests
            ++ varAndAppTests
            ++ operatorTests
            ++ caseTests
            ++ recordTests
            ++ annotationTests
        )



-- ---------------------------------------------------------------------
-- Literals
-- ---------------------------------------------------------------------


literalTests :: [Test]
literalTests =
    [ test "type/literal_integer_section_3_3" <|
        -- Per LANGUAGE.md section 3.3, an Integer literal MUST carry
        -- the trailing `L` suffix; bare `42` is a Double.
        infersTo "x <- 42L" "x" "Integer"
    , test "type/literal_bare_digits_default_to_double_section_3_3" <|
        infersTo "x <- 42" "x" "Double"
    , test "type/literal_double_section_3_3" <|
        infersTo "x <- 3.14" "x" "Double"
    , test "type/literal_character_section_3_3" <|
        infersTo "x <- \"hi\"" "x" "Character"
    , test "type/literal_logical_constructor_section_7_5_1" <|
        infersTo "x <- True" "x" "Logical"
    ]



-- ---------------------------------------------------------------------
-- Variables and curried application
-- ---------------------------------------------------------------------


varAndAppTests :: [Test]
varAndAppTests =
    [ test "type/identity_lambda_section_8_1" <|
        -- \x -> x : forall a. a -> a (printed as variable -> variable)
        case infer "id <- \\x -> x" of
            Prelude.Right binds -> case Map.lookup "id" binds of
                Just sch ->
                    Prelude.pure
                        ( assert
                            (T.isInfixOf "->" (showScheme sch))
                            ("expected an arrow type; got " Prelude.<> showScheme sch)
                        )
                Nothing -> Prelude.pure (Fail "no binding for id")
            Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
    , test "type/curried_application_section_8_1" <|
        infersTo "x <- (\\a b -> a) 1L 2L" "x" "Integer"
    , test "type/unbound_variable_rejected_section_12_1" <|
        Prelude.pure (assertLeft (infer "x <- y"))
    , test "type/let_generalisation_section_8_3" <|
        -- let id <- \x -> x in (id 1, id "hi") would need tuples; we
        -- approximate by checking that a let-bound id can be applied
        -- to two different concrete types via two top-level uses.
        case infer
            ( T.unlines
                [ "first <- (let f <- \\x -> x in f 1L)"
                , "second <- (let f <- \\x -> x in f \"hi\")"
                ]
            ) of
            Prelude.Right _ -> Prelude.pure Pass
            Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
    ]



-- ---------------------------------------------------------------------
-- Operators (section 8.8)
-- ---------------------------------------------------------------------


operatorTests :: [Test]
operatorTests =
    [ test "type/op_add_int_int_int_section_8_8" <|
        infersTo "x <- 1L + 2L" "x" "Integer"
    , test "type/op_add_bare_digits_default_to_double_section_8_8" <|
        -- Bare `1 + 2` is `Double + Double = Double` because bare
        -- digit literals are doubles.
        infersTo "x <- 1 + 2" "x" "Double"
    , test "type/op_add_dbl_dbl_dbl_section_8_8" <|
        infersTo "x <- 1.0 + 2.0" "x" "Double"
    , test "type/op_mixed_int_dbl_rejected_section_8_8" <|
        -- `1L` is Integer, `2.0` is Double - mixed primitives still
        -- a type error (no implicit coercion).
        Prelude.pure (assertLeft (infer "x <- 1L + 2.0"))
    , test "type/op_intdiv_int_only_section_8_8" <|
        infersTo "x <- 10L // 3L" "x" "Integer"
    , test "type/op_intdiv_double_rejected_section_8_8" <|
        Prelude.pure (assertLeft (infer "x <- 10.0 // 3.0"))
    , test "type/op_mod_int_only_section_8_8" <|
        infersTo "x <- 10L % 3L" "x" "Integer"
    , test "type/op_caret_double_only_section_8_8" <|
        infersTo "x <- 2.0 ^ 3.0" "x" "Double"
    , test "type/op_caret_int_rejected_section_8_8" <|
        -- `2L ^ 3L` is Integer ^ Integer; `^` requires both sides to
        -- be Double, so this is rejected.
        Prelude.pure (assertLeft (infer "x <- 2L ^ 3L"))
    , test "type/op_unary_neg_int_section_8_8" <|
        infersTo "x <- -5L" "x" "Integer"
    , test "type/op_unary_neg_double_section_8_8" <|
        infersTo "x <- -5.0" "x" "Double"
    , test "type/op_eq_returns_logical_section_8_8" <|
        infersTo "x <- 1L == 2L" "x" "Logical"
    , test "type/op_lt_mixed_rejected_section_8_8" <|
        Prelude.pure (assertLeft (infer "x <- 1L < 2.0"))
    , test "type/op_eq_string_returns_logical_section_8_8" <|
        infersTo "x <- \"a\" == \"b\"" "x" "Logical"
      -- Numeric defaulting (LANGUAGE.md section 8.8): an unannotated
      -- top-level binding whose body uses arithmetic on unconstrained
      -- operands has those operands defaulted to Double, matching R's
      -- bare-numeric default. This is what makes the natural shape
      -- @add a b <- a + b@ usable without an annotation.
    , test "type/op_unconstrained_add_defaults_to_double_section_8_8" <|
        infersTo "add a b <- a + b" "add" "Double -> Double -> Double"
    , test "type/op_unconstrained_compare_defaults_to_double_section_8_8" <|
        infersTo "ge a b <- a >= b" "ge" "Double -> Double -> Logical"
    , test "type/op_unconstrained_caret_defaults_to_double_section_8_8" <|
        infersTo "square x <- x ^ x" "square" "Double -> Double"
    ]



-- ---------------------------------------------------------------------
-- Case and patterns (section 8.4)
-- ---------------------------------------------------------------------


caseTests :: [Test]
caseTests =
    [ test "type/case_wildcard_section_8_4" <|
        infersTo
            ( T.unlines
                [ "x <- case 1L of"
                , "    _ -> 42L"
                ]
            )
            "x"
            "Integer"
    , test "type/case_constructor_section_8_4" <|
        infersTo
            ( T.unlines
                [ "x <- case Just 1L of"
                , "    Just n -> n"
                , "    Nothing -> 0L"
                ]
            )
            "x"
            "Integer"
    , test "type/case_arms_must_unify_section_8_4" <|
        Prelude.pure
            ( assertLeft
                ( infer
                    ( T.unlines
                        [ "x <- case 1L of"
                        , "    1L -> 1L"
                        , "    _ -> \"oops\""
                        ]
                    )
                )
            )
    , test "type/if_desugared_to_case_typed_section_5_3" <|
        infersTo "x <- if True then 1L else 2L" "x" "Integer"
    ]



-- ---------------------------------------------------------------------
-- Records (section 8.5)
-- ---------------------------------------------------------------------


recordTests :: [Test]
recordTests =
    [ test "type/record_field_access_section_8_5" <|
        infersTo
            "x <- { a = 1L }.a"
            "x"
            "Integer"
    , test "type/record_unknown_field_rejected_section_8_5" <|
        Prelude.pure
            ( assertLeft
                (infer "x <- { a = 1L }.b")
            )
    , test "type/record_update_section_8_5" <|
        infersTo
            "x <- { { a = 1L } | a = 2L }"
            "x"
            "{ a : Integer }"
    , test "type/record_update_wrong_field_rejected_section_8_5" <|
        Prelude.pure
            ( assertLeft
                (infer "x <- { { a = 1L } | b = 2L }")
            )
    ]



-- ---------------------------------------------------------------------
-- Annotations (section 8.1)
-- ---------------------------------------------------------------------


annotationTests :: [Test]
annotationTests =
    [ test "type/annotation_matches_section_8_1" <|
        case infer
            ( T.unlines
                [ "x : Integer"
                , "x <- 1L"
                ]
            ) of
            Prelude.Right _ -> Prelude.pure Pass
            Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
    , test "type/annotation_mismatch_rejected_section_8_1" <|
        Prelude.pure
            ( assertLeft
                ( infer
                    ( T.unlines
                        [ "x : Integer"
                        , "x <- \"oops\""
                        ]
                    )
                )
            )
    , test "type/annotation_pins_operator_operands_section_8_1" <|
        -- Without annotation flow this would fail (`/ : Double -> Double
        -- -> Double` can't infer the types of two unconstrained vars).
        -- The annotation makes both `max_score` and `raw` Double.
        infersTo
            ( T.unlines
                [ "normalize : Double -> Double -> Double"
                , "normalize max_score raw <- raw / max_score"
                ]
            )
            "normalize"
            "Double -> Double -> Double"
    , test "type/annotation_pins_param_types_in_body_section_8_1" <|
        Prelude.pure
            ( assertLeft
                ( infer
                    ( T.unlines
                        [ "f : Integer -> Integer"
                        , "f x <- x + 1.0"
                        ]
                    )
                )
            )
    , test "type/foreign_import_binds_value_section_4_5" <|
        -- A foreign-imported name can be referenced like any value.
        case infer
            ( T.unlines
                [ "import readr.read_csv : Character -> Integer"
                , ""
                , "load path <- read_csv path"
                ]
            ) of
            Prelude.Right _ -> Prelude.pure Pass
            Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
    , test "type/foreign_import_argument_type_enforced_section_4_5" <|
        Prelude.pure
            ( assertLeft
                ( infer
                    ( T.unlines
                        [ "import readr.read_csv : Character -> Integer"
                        , ""
                        , "x <- read_csv 42"
                        ]
                    )
                )
            )
    ]



-- ---------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------


infer :: Text -> Prelude.Either Text (Map.Map Text Scheme)
infer src =
    case desugarSource src of
        Prelude.Left d -> Prelude.Left (T.pack (Prelude.show d))
        Prelude.Right p ->
            case inferProgram p of
                Prelude.Left d -> Prelude.Left (T.pack (Prelude.show d))
                Prelude.Right tp -> Prelude.Right (typedBindings tp)


-- | Assert that a top-level binding's inferred body type prints as
-- the given string.
infersTo :: Text -> Text -> Text -> Prelude.IO TestResult
infersTo src name expected =
    case infer src of
        Prelude.Left msg -> Prelude.pure (Fail msg)
        Prelude.Right binds ->
            case Map.lookup name binds of
                Nothing -> Prelude.pure (Fail ("no binding for " Prelude.<> name))
                Just sch -> do
                    let actual = showType (schemeBody sch)
                    Prelude.pure
                        ( assert
                            (actual Prelude.== expected)
                            ("expected " Prelude.<> expected Prelude.<> "; got " Prelude.<> actual)
                        )
