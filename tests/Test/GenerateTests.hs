module Test.GenerateTests (suite) where

import qualified Data.Text as T
import NriPrelude
import Quone.Cli.Commands (compileScript)
import qualified Test.Harness as Harness
import Test.Harness (TestResult (..), (===))
import qualified Prelude


suite :: Harness.Suite
suite =
    Harness.describe "generate"
        [ Harness.test "generate/double_literals_are_idiomatic" <|
            expectGenerated "x <- 1" "x <- 1"
        , Harness.test "generate/record_literal" <|
            expectGenerated
                "student <- { name = \"Ada\", score = 95 }"
                "student <- list(name = \"Ada\", score = 95)"
        , Harness.test "generate/foreign_import_via" <|
            expectGenerated
                ( T.unlines
                    [ "import elementwise stringr.str_detect as str_detect : Character -> Character -> Logical via \"stringr::str_detect($2, $1)\""
                    , "x <- str_detect \"Ada\" \"Ada Lovelace\""
                    ]
                )
                "x <- stringr::str_detect(\"Ada Lovelace\", \"Ada\")"
        , Harness.test "generate/dataframe_pipeline" <|
            expectGeneratedContains
                "passing <- dataframe { name = [\"Ada\"], score = [90] } |> filter (score > 80)"
                "dplyr::filter(score > 80)"
        ]


expectGenerated :: Text -> Text -> Prelude.IO TestResult
expectGenerated src expected =
    case compileScript "<test>" src of
        Prelude.Right (_, r) -> Prelude.pure (r === expected)
        Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))


expectGeneratedContains :: Text -> Text -> Prelude.IO TestResult
expectGeneratedContains src expected =
    case compileScript "<test>" src of
        Prelude.Right (_, r) ->
            if expected `T.isInfixOf` r then
                Prelude.pure Pass
            else
                Prelude.pure (Fail ("expected generated R to contain: " ++ expected ++ "\nactual:\n" ++ r))
        Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))

