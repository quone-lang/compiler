module Test.ParseTests (suite) where

import NriPrelude
import qualified Data.Text as T
import Quone.Lex.Token (Keyword (..))
import Quone.Parse.Cst
import Quone.Parse.Parser (parseProgram)
import qualified Test.Harness as Harness
import Test.Harness (TestResult (..), (===))
import qualified Prelude


suite :: Harness.Suite
suite =
    Harness.describe "parser"
        [ Harness.test "parse/function_definition" <|
            case parseProgram "add x y <- x + y" of
                Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                    Prelude.pure (Prelude.length (valueDeclParams v) === 2)
                other -> Prelude.pure (Fail ("unexpected parse result: " ++ showText other))
        , Harness.test "parse/join_without_on_keyword" <|
            case parseProgram "joined <- students |> inner_join schools { school_id = id }" of
                Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                    case valueDeclBody v of
                        CEPipe _ _ (CEVerb _ KInnerJoin [CDAJoinOn _ _ pairs]) ->
                            Prelude.pure (Prelude.length pairs === 1)
                        other -> Prelude.pure (Fail ("unexpected body: " ++ showText other))
                other -> Prelude.pure (Fail ("unexpected parse result: " ++ showText other))
        , Harness.test "parse/deferred_verb_not_reserved" <|
            case parseProgram "full_join <- 1" of
                Prelude.Right (CProgram {programDecls = [CDValue v]}) ->
                    Prelude.pure (lowerNameText (valueDeclName v) === "full_join")
                other -> Prelude.pure (Fail ("unexpected parse result: " ++ showText other))
        , Harness.test "parse/multiline_custom_type" <|
            case parseProgram (T.unlines ["type A", "    <- First", "    | Second", "", "b <- First"]) of
                Prelude.Right (CProgram {programDecls = [CDType td, CDValue v]}) ->
                    Prelude.pure
                        ( (upperNameText (typeDeclName td), lowerNameText (valueDeclName v))
                            === ("A", "b")
                        )
                other -> Prelude.pure (Fail ("unexpected parse result: " ++ showText other))
        , Harness.test "parse/rejects_indented_top_level_declaration" <|
            case parseProgram (T.unlines ["type A", "    <- First", "", "b <- First", "    b <- First"]) of
                Prelude.Left _ -> Prelude.pure Pass
                other -> Prelude.pure (Fail ("expected parse failure, got: " ++ showText other))
        ]


showText :: Prelude.Show a => a -> Text
showText = T.pack Prelude.. Prelude.show

