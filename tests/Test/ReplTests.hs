{-| REPL meta-command and session tests.

Compiler track A5 (project plan): the REPL parses a small
meta-command grammar (@:type@, @:load@, @:reload@, @:browse@,
@:quit@, @:help@, plus aliases) and otherwise hands the input to
the language pipeline. This module covers the pure parser and the
session bookkeeping; the end-to-end @Rscript@ integration test is
deferred until CI grows an R install.

-}
module Test.ReplTests (suite) where

import qualified Data.Text as T
import NriPrelude
import qualified Quone.Repl.Meta as Meta
import qualified Quone.Repl.Session as Session
import Test.Harness
    ( Suite
    , Test
    , TestResult (..)
    , describe
    , test
    , (===)
    )
import qualified Prelude



suite :: Suite
suite =
    describe
        "repl"
        ( metaTests
            ++ sessionTests
        )


metaTests :: [Test]
metaTests =
    [ test "repl/parses_quit_section_a5"
        (Prelude.pure (Meta.parseMeta ":quit" === Just Meta.MQuit))
    , test "repl/parses_quit_alias_section_a5"
        (Prelude.pure (Meta.parseMeta ":q" === Just Meta.MQuit))
    , test "repl/parses_help_section_a5"
        (Prelude.pure (Meta.parseMeta ":help" === Just Meta.MHelp))
    , test "repl/parses_help_alias_section_a5"
        (Prelude.pure (Meta.parseMeta ":?" === Just Meta.MHelp))
    , test "repl/parses_browse_section_a5"
        (Prelude.pure (Meta.parseMeta ":browse" === Just Meta.MBrowse))
    , test "repl/parses_reload_section_a5"
        (Prelude.pure (Meta.parseMeta ":reload" === Just Meta.MReload))
    , test "repl/parses_type_section_a5"
        ( Prelude.pure
            (Meta.parseMeta ":type x" === Just (Meta.MType "x")))
    , test "repl/parses_type_alias_section_a5"
        ( Prelude.pure
            (Meta.parseMeta ":t x" === Just (Meta.MType "x")))
    , test "repl/parses_load_section_a5"
        ( Prelude.pure
            (Meta.parseMeta ":load src/Main.Q"
                === Just (Meta.MLoad "src/Main.Q")))
    , test "repl/identifies_unknown_section_a5"
        ( Prelude.pure
            (Meta.parseMeta ":wat"
                === Just (Meta.MUnknown "wat")))
    , test "repl/non_meta_returns_nothing_section_a5"
        (Prelude.pure (Meta.parseMeta "x <- 1" === Prelude.Nothing))
    ]


sessionTests :: [Test]
sessionTests =
    [ test "repl/empty_session_browses_to_nothing_section_a5"
        ( Prelude.pure
            (Session.browse (Session.empty Prelude.Nothing) === []))
    , test "repl/type_lookup_for_unknown_returns_planned_message_section_a5"
        ( do
            let
                msg =
                    Session.inferType
                        (Session.empty Prelude.Nothing)
                        "missing"
            Prelude.pure
                ( if T.isInfixOf "[planned]" msg
                    then Pass
                    else
                        Fail
                            ("expected hint about [planned]; got: "
                                ++ msg)
                )
        )
    ]
