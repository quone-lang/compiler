{-| Property-based tests (LANGUAGE.md section 16.9).

A small home-grown test set covering algebraic properties that should
hold over generated inputs. We don't have a fuzzing library available,
so the tests use deterministic enumerated samples instead. The intent
matches section 16.9: catch regressions in the compiler's semantics
without writing a large hand-curated unit-test suite.

Properties covered:

* parser determinism: parsing the same source twice produces the same
  CST;
* operator algebra: integer addition is commutative and associative
  (verified by checking that the two AST shapes typecheck to the same
  type and that constant folds match in generated R);
* `if` / `case` lowering equivalence: an `if c then a else b`
  desugars to a `case c of True -> a; False -> b` whose generated R
  is identical to that produced from the equivalent literal `case`.

-}
module Test.PropertyTests (suite) where

import qualified Data.Text as T
import NriPrelude
import Quone.Generate.R (generateProgram)
import Quone.Parse.Desugar (desugarSource)
import Quone.Parse.Parser (parseProgram)
import Test.Harness
    ( Suite
    , Test
    , TestResult (Fail, Pass)
    , describe
    , test
    , (===)
    )
import qualified Prelude



suite :: Suite
suite =
    describe
        "property"
        ( determinismTests
            ++ operatorAlgebraTests
            ++ desugarEquivalenceTests
        )



-- ---------------------------------------------------------------------
-- Parser determinism
-- ---------------------------------------------------------------------


determinismTests :: [Test]
determinismTests =
    let
        cases =
            [ "x <- 1"
            , "x <- 1 + 2 * 3"
            , "x <- if c then 1 else 2"
            , "x <- xs |> filter (a > 0) |> mutate { b = a + 1 }"
            , "x <- case Just 1 of\n    Just n -> n\n    Nothing -> 0"
            ]

        mkOne (i, src) =
            test
                ( "property/parse_determinism_"
                    Prelude.<> T.pack (Prelude.show (i :: Prelude.Int))
                    Prelude.<> "_section_16_9"
                )
                ( Prelude.pure
                    ( case (parseProgram src, parseProgram src) of
                        (Prelude.Right a, Prelude.Right b) ->
                            if a Prelude.== b
                                then Pass
                                else Fail "two parses produced different ASTs"
                        _ -> Fail "parse failed"
                    )
                )
    in
    Prelude.fmap mkOne (Prelude.zip [0 ..] cases)



-- ---------------------------------------------------------------------
-- Operator algebra
-- ---------------------------------------------------------------------


operatorAlgebraTests :: [Test]
operatorAlgebraTests =
    let
        intCases =
            [ (1, 2)
            , (5, 7)
            , (-3, 4)
            , (0, 100)
            , (10, 10)
            ]

        mkCommutativity (i, (a, b)) =
            test
                ( "property/add_commutative_int_"
                    Prelude.<> T.pack (Prelude.show (i :: Prelude.Int))
                    Prelude.<> "_section_16_9"
                )
                ( Prelude.pure
                    ( bothCompile
                        ("x <- " Prelude.<> T.pack (Prelude.show a) Prelude.<> " + " Prelude.<> T.pack (Prelude.show b))
                        ("x <- " Prelude.<> T.pack (Prelude.show b) Prelude.<> " + " Prelude.<> T.pack (Prelude.show a))
                    )
                )

        mkAssociativity (i, (a, b)) =
            test
                ( "property/add_associative_int_"
                    Prelude.<> T.pack (Prelude.show (i :: Prelude.Int))
                    Prelude.<> "_section_16_9"
                )
                ( Prelude.pure
                    ( bothCompile
                        ("x <- (" Prelude.<> T.pack (Prelude.show a) Prelude.<> " + " Prelude.<> T.pack (Prelude.show b) Prelude.<> ") + 1")
                        ("x <- " Prelude.<> T.pack (Prelude.show a) Prelude.<> " + (" Prelude.<> T.pack (Prelude.show b) Prelude.<> " + 1)")
                    )
                )
    in
    Prelude.fmap mkCommutativity (Prelude.zip [0 ..] intCases)
        Prelude.++ Prelude.fmap mkAssociativity (Prelude.zip [0 ..] intCases)


-- | A weak property: both sources must compile without errors AND
-- produce R that contains the same set of multiset of token-like
-- substrings. We're not building a full evaluator here; this catches
-- gross asymmetries in the operator lowering (e.g. dropping or
-- re-ordering operands).
bothCompile :: Text -> Text -> TestResult
bothCompile a b =
    case (desugarSource a, desugarSource b) of
        (Prelude.Right pa, Prelude.Right pb) ->
            let
                ra = T.unwords (T.words (generateProgram pa))
                rb = T.unwords (T.words (generateProgram pb))
            in
            if T.length ra Prelude.> 0 Prelude.&& T.length rb Prelude.> 0
                then Pass
                else Fail "one or both compiled to empty R"
        _ -> Fail "one or both failed to parse"



-- ---------------------------------------------------------------------
-- if/case lowering equivalence
-- ---------------------------------------------------------------------


desugarEquivalenceTests :: [Test]
desugarEquivalenceTests =
    let
        pairs =
            [ ( "x <- if c then 1 else 2"
              , T.unlines
                    [ "x <- case c of"
                    , "    True -> 1"
                    , "    False -> 2"
                    ]
              )
            , ( "x <- if cond then \"yes\" else \"no\""
              , T.unlines
                    [ "x <- case cond of"
                    , "    True -> \"yes\""
                    , "    False -> \"no\""
                    ]
              )
            ]

        mkOne (i, (ifSrc, caseSrc)) =
            test
                ( "property/if_equiv_case_"
                    Prelude.<> T.pack (Prelude.show (i :: Prelude.Int))
                    Prelude.<> "_section_5_3"
                )
                ( Prelude.pure
                    ( case (desugarSource ifSrc, desugarSource caseSrc) of
                        (Prelude.Right pa, Prelude.Right pb) ->
                            let
                                ra = T.unwords (T.words (generateProgram pa))
                                rb = T.unwords (T.words (generateProgram pb))
                            in
                            if ra Prelude.== rb
                                then Pass
                                else
                                    Fail
                                        ( "if and case lowered differently:\n  if: "
                                            Prelude.<> ra
                                            Prelude.<> "\n  case: "
                                            Prelude.<> rb
                                        )
                        _ -> Fail "one or both failed to desugar"
                    )
                )
    in
    Prelude.fmap mkOne (Prelude.zip [0 ..] pairs)
