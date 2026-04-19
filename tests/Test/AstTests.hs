{-| AST and validation tests.

Covers stage 4 of [docs/PLAN.md](../docs/PLAN.md):

* the desugaring from LANGUAGE.md section 5.3 (@if@ ⇒ @case@) is
  observed at the AST level: there is no 'EIf' constructor and the
  parser-CST 'CEIf' lowers to 'ECase' on @True@ / @False@;
* well-formedness invariants from section 6.8 that this stage owns
  (export consistency, R-export discipline);
* spot-check that the AST mirrors the CST shape for the common
  declaration kinds.

-}
module Test.AstTests (suite) where

import qualified Data.Text as T
import NriPrelude
import Quone.Ast.Source
import Quone.Ast.Validate (validate)
import Quone.Parse.Desugar (desugarSource)
import Test.Harness
    ( Suite
    , Test
    , TestResult (Fail, Pass)
    , assert
    , describe
    , test
    , (===)
    )
import qualified Prelude



suite :: Suite
suite =
    describe
        "ast"
        ( desugarTests
            ++ validationTests
        )



-- ---------------------------------------------------------------------
-- Desugaring
-- ---------------------------------------------------------------------


desugarTests :: [Test]
desugarTests =
    [ test "ast/if_desugars_to_case_section_5_3" <|
        case desugarSource "x <- if c then 1 else 2" of
            Prelude.Right p -> case Prelude.head (programDecls p) of
                DValue v -> case valueDeclBody v of
                    ECase _ _ arms ->
                        Prelude.pure
                            ( assert
                                (Prelude.length arms Prelude.== 2 Prelude.&&
                                    Prelude.all (\a -> isLogicalCon (caseArmPattern a)) arms)
                                ("expected 2 case arms with True/False patterns; got " Prelude.<> T.pack (Prelude.show arms))
                            )
                    other ->
                        Prelude.pure (Fail ("expected ECase; got " Prelude.<> T.pack (Prelude.show other)))
                _ -> Prelude.pure (Fail "first decl was not DValue")
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "ast/no_eif_constructor_section_6_8_invariant_9" <|
        -- This is structurally guaranteed: the AST has no EIf
        -- constructor. We assert the desugar produces a non-EIf shape.
        case desugarSource "x <- if c then 1 else 2" of
            Prelude.Right p -> case Prelude.head (programDecls p) of
                DValue v ->
                    Prelude.pure
                        ( assert
                            (case valueDeclBody v of
                                ECase _ _ _ -> Prelude.True
                                _ -> Prelude.False)
                            "if must lower to ECase, never EIf"
                        )
                _ -> Prelude.pure (Fail "first decl was not DValue")
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "ast/paren_dropped_section_6_4" <|
        case desugarSource "x <- (1 + 2)" of
            Prelude.Right p -> case Prelude.head (programDecls p) of
                DValue v -> case valueDeclBody v of
                    EBinOp _ OpAdd _ _ -> Prelude.pure Pass
                    other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
                _ -> Prelude.pure (Fail "first decl was not DValue")
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "ast/verb_keyword_translates_section_6_6" <|
        case desugarSource "x <- students |> filter (score > 70.0)" of
            Prelude.Right p -> case Prelude.head (programDecls p) of
                DValue v -> case valueDeclBody v of
                    EPipe _ _ (EVerb _ VFilter _) -> Prelude.pure Pass
                    other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
                _ -> Prelude.pure (Fail "first decl was not DValue")
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    ]


isLogicalCon :: Pattern -> Prelude.Bool
isLogicalCon = \case
    PCon _ n [] -> upperText n Prelude.== "True" Prelude.|| upperText n Prelude.== "False"
    _ -> Prelude.False



-- ---------------------------------------------------------------------
-- Validation invariants
-- ---------------------------------------------------------------------


validationTests :: [Test]
validationTests =
    [ test "ast/validate_export_consistency_pos_section_6_8_inv2" <|
        let
            src = T.unlines
                [ "module Foo exporting (x)"
                , ""
                , "x <- 1"
                ]
        in
        case desugarSource src of
            Prelude.Right p ->
                Prelude.pure
                    ( assert
                        (Prelude.null (validate p))
                        ("expected no diagnostics; got " Prelude.<> T.pack (Prelude.show (validate p)))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "ast/validate_export_consistency_neg_section_6_8_inv2" <|
        let
            src = T.unlines
                [ "module Foo exporting (missing)"
                , ""
                , "x <- 1"
                ]
        in
        case desugarSource src of
            Prelude.Right p ->
                Prelude.pure
                    ( assert
                        (Prelude.not (Prelude.null (validate p)))
                        "expected a diagnostic about the missing export"
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "ast/validate_r_export_discipline_pos_section_6_8_inv4" <|
        let
            src = T.unlines
                [ "module Foo exporting (x)"
                , ""
                , "#' @export"
                , "x <- 1"
                ]
        in
        case desugarSource src of
            Prelude.Right p ->
                Prelude.pure
                    ( assert
                        (Prelude.null (validate p))
                        ("expected no diagnostics; got " Prelude.<> T.pack (Prelude.show (validate p)))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "ast/validate_r_export_discipline_neg_section_6_8_inv4" <|
        let
            src = T.unlines
                [ "module Foo exporting (y)"
                , ""
                , "#' @export"
                , "x <- 1"
                , ""
                , "y <- 2"
                ]
        in
        case desugarSource src of
            Prelude.Right p ->
                Prelude.pure
                    ( assert
                        (Prelude.not (Prelude.null (validate p)))
                        "expected a diagnostic about @export on a non-exported binding"
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    , test "ast/validate_wildcard_export_skips_consistency_check_section_6_8_inv2" <|
        let
            src = T.unlines
                [ "module Foo exporting (..)"
                , ""
                , "x <- 1"
                ]
        in
        case desugarSource src of
            Prelude.Right p ->
                Prelude.pure
                    ( assert
                        (Prelude.null (validate p))
                        ("wildcard exports never fail consistency; got " Prelude.<> T.pack (Prelude.show (validate p)))
                    )
            other -> Prelude.pure (Fail (T.pack (Prelude.show other)))
    ]
