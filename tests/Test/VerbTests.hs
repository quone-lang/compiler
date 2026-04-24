module Test.VerbTests (suite) where

import qualified Data.Text as T
import NriPrelude
import Quone.Cli.Commands (compileScript)
import qualified Test.Harness as Harness
import Test.Harness (TestResult (..))
import qualified Prelude


suite :: Harness.Suite
suite =
    Harness.describe "verbs"
        [ Harness.test "verb/filter_allows_reducer_in_predicate" <|
            expectOk
                ( T.unlines
                    [ "students <- dataframe { name = [\"Ada\", \"Bob\"], score = [90, 70] }"
                    , "above_avg <- students |> filter (score > mean score)"
                    ]
                )
        , Harness.test "verb/mutate_allows_reducer_broadcast" <|
            expectOk
                ( T.unlines
                    [ "students <- dataframe { score = [90, 70] }"
                    , "centered <- students |> mutate { centered = score - mean score }"
                    ]
                )
        , Harness.test "verb/summarize_allows_scalar_constant" <|
            expectOk
                ( T.unlines
                    [ "students <- dataframe { score = [90, 70] }"
                    , "summary <- students |> summarize { label = \"all\", avg = mean score }"
                    ]
                )
        , Harness.test "verb/rename_preserves_pipeline" <|
            expectOk
                ( T.unlines
                    [ "students <- dataframe { name = [\"Ada\"], score = [90] }"
                    , "renamed <- students |> rename { student_name = name }"
                    ]
                )
        , Harness.test "verb/rejects_unknown_column" <|
            expectFail
                ( T.unlines
                    [ "students <- dataframe { name = [\"Ada\"] }"
                    , "bad <- students |> select { missing }"
                    ]
                )
        ]


expectOk :: Text -> Prelude.IO TestResult
expectOk src =
    case compileScript "<test>" src of
        Prelude.Right _ -> Prelude.pure Pass
        Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))


expectFail :: Text -> Prelude.IO TestResult
expectFail src =
    case compileScript "<test>" src of
        Prelude.Left _ -> Prelude.pure Pass
        Prelude.Right _ -> Prelude.pure (Fail "expected failure")

