module Test.FormatTests (suite) where

import qualified Data.Text as T
import NriPrelude
import qualified Quone.Format.Format as Format
import qualified Test.Harness as Harness
import Test.Harness (TestResult (..), (===))
import qualified Prelude


suite :: Harness.Suite
suite =
    Harness.describe "format"
        [ Harness.test "format/idempotent" <|
            case Format.format "<test>" "x<-1+2" of
                Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
                Prelude.Right once ->
                    case Format.format "<test>" once of
                        Prelude.Right twice -> Prelude.pure (twice === once)
                        Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
        , Harness.test "format/preserves_line_comment" <|
            case Format.format "<test>" "# keep me\nx <- 1" of
                Prelude.Right out ->
                    if "# keep me" `T.isInfixOf` out then
                        Prelude.pure Pass
                    else
                        Prelude.pure (Fail ("comment was not preserved: " ++ out))
                Prelude.Left d -> Prelude.pure (Fail (T.pack (Prelude.show d)))
        ]

