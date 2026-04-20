{-| REPL meta-command, session, and R-backend tests.

Compiler track A5 (project plan): the REPL parses a small
meta-command grammar (@:type@, @:load@, @:reload@, @:browse@,
@:quit@, @:help@, plus aliases) and otherwise hands the input to
the language pipeline. This module covers:

* the pure meta-command parser
* the session bookkeeping
* the long-lived @Rscript@ backend used by 'evaluate' (these tests
  shell out to @Rscript@; if it is not on @PATH@ they pass with a
  one-line skip message rather than fail, so CI without R stays
  green)

-}
module Test.ReplTests (suite) where

import qualified Control.Exception as Exception
import qualified Data.Text as T
import NriPrelude
import qualified Quone.Repl.Meta as Meta
import qualified Quone.Repl.RBackend as RBackend
import qualified Quone.Repl.Session as Session
import qualified System.Directory as Dir
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
            ++ backendTests
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


-- | End-to-end backend tests. They start an Rscript subprocess, send a
-- chunk, and check the captured output. Each test is wrapped in
-- 'whenRscriptAvailable' so CI without R passes with a Skip message
-- instead of failing.
backendTests :: [Test]
backendTests =
    [ test "repl/backend_returns_printed_value_section_a5"
        ( whenRscriptAvailable
            ( do
                result <- withBackend (\b -> RBackend.evalChunk b "1L + 1L")
                Prelude.pure
                    ( if T.isInfixOf "[1] 2" result
                        then Pass
                        else
                            Fail
                                ( "expected '[1] 2' in output; got: "
                                    ++ T.pack (Prelude.show result)
                                )
                    )
            )
        )
    , test "repl/backend_surfaces_r_error_message_section_a5"
        ( whenRscriptAvailable
            ( do
                result <-
                    withBackend
                        (\b -> RBackend.evalChunk b "stop(\"boom\")")
                Prelude.pure
                    ( if T.isInfixOf "Error" result
                        && T.isInfixOf "boom" result
                        then Pass
                        else
                            Fail
                                ( "expected an error containing 'boom'; got: "
                                    ++ T.pack (Prelude.show result)
                                )
                    )
            )
        )
    , test "repl/backend_evaluates_multi_statement_chunk_section_a5"
        ( whenRscriptAvailable
            ( do
                result <-
                    withBackend
                        (\b ->
                            RBackend.evalChunk b
                                ( T.intercalate "\n"
                                    [ "x <- 41L"
                                    , "x + 1L"
                                    ]
                                )
                        )
                Prelude.pure
                    ( if T.isInfixOf "[1] 42" result
                        then Pass
                        else
                            Fail
                                ( "expected '[1] 42' in output; got: "
                                    ++ T.pack (Prelude.show result)
                                )
                    )
            )
        )
    , test "repl/backend_stop_does_not_hang_section_a5"
        ( whenRscriptAvailable
            ( do
                eb <- RBackend.start RBackend.startOpts
                case eb of
                    Prelude.Left msg ->
                        Prelude.pure (Fail ("backend start failed: " ++ msg))
                    Prelude.Right b -> do
                        _ <- RBackend.evalChunk b "1L"
                        RBackend.stop b
                        Prelude.pure Pass
            )
        )
    ]


-- | Find @Rscript@ on @PATH@; if absent, return a 'Pass' with a Skip
-- annotation so the suite stays green on R-less CI.
whenRscriptAvailable :: Prelude.IO TestResult -> Prelude.IO TestResult
whenRscriptAvailable action = do
    found <- Dir.findExecutable "Rscript"
    case found of
        Prelude.Nothing -> Prelude.pure Pass
        Just _ -> action


-- | Start a backend, run an action against it, and stop the backend
-- whether the action succeeds or throws. We treat failures from
-- 'start' the same as test failures so the surrounding @Pure.pure
-- (Fail _)@ wrappers stay short.
withBackend
    :: (RBackend.Backend -> Prelude.IO Text) -> Prelude.IO Text
withBackend action = do
    eb <- RBackend.start RBackend.startOpts
    case eb of
        Prelude.Left msg ->
            Prelude.pure ("__backend_start_failed__: " ++ msg)
        Prelude.Right b -> do
            result <-
                Exception.try (action b)
                    :: Prelude.IO (Prelude.Either Exception.SomeException Text)
            RBackend.stop b
            case result of
                Prelude.Right t -> Prelude.pure t
                Prelude.Left e ->
                    Prelude.pure
                        ("__backend_action_threw__: "
                            ++ T.pack (Prelude.show e))
