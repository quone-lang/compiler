# Quone CLI - Version 0.0.1

**Status:** Normative for the v0.0.1 surface.

**Audience:** Users of the `quonec` command-line tool, IDE/editor
integrators, and CI/build-script authors. The R companion package
(`quone::repl()`, `quone::compile()`, etc.) shells out to `quonec`
under the hood, so the surface defined here also constrains the R
package.

**Document scope:** This document specifies the `quonec` command-line
surface: subcommands, cross-cutting flags, exit codes, and the
machine-readable diagnostic format used by editors and the R
companion. The language itself is defined in [LANGUAGE.md](LANGUAGE.md);
the lowering to R in [COMPILATION.md](COMPILATION.md); the formatter in
[FORMATTER.md](FORMATTER.md); the LSP and REPL surfaces in
[IDE.md](IDE.md).

---

## Table of contents

1. [Diagnostics format (Appendix A.1)](#1-diagnostics-format)
2. [Build outputs and source maps (Appendix A.2)](#2-build-outputs-and-source-maps)
3. [Subcommands (Appendix A.3)](#3-subcommands)
4. [Subcommands defined in other docs](#4-subcommands-defined-in-other-docs)

---

## 1. Diagnostics format

(Internally `section_a1`; corresponds to the `--diagnostics-format`
flag.)

`quonec` emits compiler diagnostics (errors, warnings) in one of two
formats, selected by a cross-cutting flag:

| Flag                              | Effect                                                     |
| --------------------------------- | ---------------------------------------------------------- |
| `--diagnostics-format=human`      | Default. Human-readable text on stderr.                    |
| `--diagnostics-format=json`       | One NDJSON object per diagnostic on stderr.                |
| `--json`                          | Short alias for `--diagnostics-format=json`.               |

**Human format.** A single multi-line block per diagnostic, of the
shape:

```
error[type-mismatch]: cannot unify Integer with Double
  --> src/foo.Q:3:7-12
hint: insert an explicit `to_double` conversion
```

The first line is `<severity>[<category>]: <message>` where
`<category>` is one of the values listed in
[LANGUAGE.md section 12.1](LANGUAGE.md#121-error-categories) plus
`internal` for compiler bugs. The second line is the source span. An
optional `hint:` line follows.

**JSON format.** One JSON object per line on stderr. Each object MUST
include `severity`, `category`, `message`, `span`, and (optionally)
`hint` keys. The exact schema is normative for editor integrations;
see [LANGUAGE.md section 12.1](LANGUAGE.md#121-error-categories) for
category names, and the `quonec --diagnostics-format=json check` tests
for span shape.

The cross-cutting flag MAY appear before or after the subcommand:

```
quonec --json check src/foo.Q
quonec check --json src/foo.Q
quonec check src/foo.Q --json
```

All three forms are equivalent.

<!-- BEGIN tests:a1 -->
**Tests** (4):

- [`cli/check_defaults_to_human_section_a1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/CliTests.hs#L64)
- [`cli/check_accepts_json_section_a1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/CliTests.hs#L68)
- [`cli/check_accepts_human_explicit_section_a1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/CliTests.hs#L72)
- [`cli/check_accepts_short_json_section_a1`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/CliTests.hs#L77)

<details>
<summary>Show test source</summary>

```haskell
    test "cli/check_defaults_to_human_section_a1"
        ( Prelude.pure
            (parseArgs ["check", "src/Foo.Q"]
                === CmdCheck HumanDiagnostics "src/Foo.Q"))
    test "cli/check_accepts_json_section_a1"
        ( Prelude.pure
            (parseArgs ["check", "--diagnostics-format=json", "src/Foo.Q"]
                === CmdCheck JsonDiagnostics "src/Foo.Q"))
    test "cli/check_accepts_human_explicit_section_a1"
        ( Prelude.pure
            (parseArgs
                ["check", "--diagnostics-format=human", "src/Foo.Q"]
                === CmdCheck HumanDiagnostics "src/Foo.Q"))
    test "cli/check_accepts_short_json_section_a1"
        ( Prelude.pure
            (parseArgs ["check", "--json", "src/Foo.Q"]
                === CmdCheck JsonDiagnostics "src/Foo.Q"))
```

</details>
<!-- END tests:a1 -->

---

## 2. Build outputs and source maps

(Internally `section_a2`; corresponds to the `--emit-sourcemap` flag
and the `.R.map` sidecar format.)

`quonec build` writes a `.R` file alongside its `.Q` input. With
`--emit-sourcemap`, it additionally writes a `.R.map` sidecar
mapping every generated R line back to the source `.Q` position it
came from. The sidecar uses a small line-oriented format:

```
{ "version": 1, "sources": ["src/foo.Q"], "mappings": [ ... ] }
```

Each `mappings` entry is a JSON object with keys `r` (the R line and
column) and `q` (the source line and column). The format is intended
for editor source-jumping and for the LSP server's "go to definition
on a generated R line" feature; it is NOT a standard JS source-map
format.

The `--out=DIR` cross-cutting flag overrides the directory where
`.R` and `.R.map` are written. Without it, both land next to the
`.Q` source.

<!-- BEGIN tests:a2 -->
**Tests** (6):

- [`cli/build_accepts_emit_sourcemap_section_a2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/CliTests.hs#L97)
- [`sourcemap/one_entry_per_decl_section_a2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/SourceMapTests.hs#L49)
- [`sourcemap/maps_first_decl_to_line_1_section_a2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/SourceMapTests.hs#L66)
- [`sourcemap/header_includes_paths_section_a2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/SourceMapTests.hs#L85)
- [`sourcemap/entry_uses_r_and_q_keys_section_a2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/SourceMapTests.hs#L100)
- [`sourcemap/entry_columns_round_trip_section_a2`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/SourceMapTests.hs#L116)

<details>
<summary>Show test source</summary>

```haskell
    test "cli/build_accepts_emit_sourcemap_section_a2"
        ( Prelude.pure
            (parseArgs ["build", "--emit-sourcemap", "src/Foo.Q"]
                === CmdBuildScript
                    defaultBuildOpts
                        {boSourcemap = Prelude.True}
                    "src/Foo.Q"))
    test "sourcemap/one_entry_per_decl_section_a2"
        ( do
            let
                src =
                    T.unlines
                        [ "x <- 1"
                        , "y <- 2"
                        , "z <- x + y"
                        ]
            case desugarSource src of
                Prelude.Left _ ->
                    Prelude.pure (Fail "parse failed")
                Prelude.Right prog -> do
                    let
                        entries = buildSourceMap prog
                    Prelude.pure (Prelude.length entries === 3)
        )
    test "sourcemap/maps_first_decl_to_line_1_section_a2"
        ( do
            let
                src = "x <- 1"
            case desugarSource src of
                Prelude.Left _ ->
                    Prelude.pure (Fail "parse failed")
                Prelude.Right prog -> do
                    case buildSourceMap prog of
                        (e : _) ->
                            Prelude.pure (entryQLine e === 1)
                        [] ->
                            Prelude.pure (Fail "no entries")
        )
    test "sourcemap/header_includes_paths_section_a2"
        ( do
            let
                sm =
                    SourceMap
                        { smGenerated = "out/foo.R"
                        , smSource = "src/foo.Q"
                        , smEntries = []
                        }
                encoded = encodeSourceMap sm
            Prelude.pure
                (assertCond
                    "header has source path"
                    (T.isInfixOf "\"source\":\"src/foo.Q\"" encoded))
        )
    test "sourcemap/entry_uses_r_and_q_keys_section_a2"
        ( do
            let
                sm =
                    SourceMap
                        { smGenerated = "out.R"
                        , smSource = "in.Q"
                        , smEntries = [Entry 1 1 5 7]
                        }
                encoded = encodeSourceMap sm
            Prelude.pure
                (assertCond
                    "entry has r and q sub-objects"
                    (T.isInfixOf "\"r\":{" encoded
                        Prelude.&& T.isInfixOf "\"q\":{" encoded))
        )
    test "sourcemap/entry_columns_round_trip_section_a2"
        ( do
            let
                sm =
                    SourceMap
                        { smGenerated = "out.R"
                        , smSource = "in.Q"
                        , smEntries = [Entry 2 1 9 12]
                        }
                encoded = encodeSourceMap sm
            Prelude.pure
                (assertCond
                    "encodes line/col"
                    (T.isInfixOf "\"line\":9" encoded
                        Prelude.&& T.isInfixOf "\"col\":12" encoded))
        )
```

</details>
<!-- END tests:a2 -->

---

## 3. Subcommands

(Internally `section_a3`.)

The full subcommand list, drawn from `quonec --help`:

```
quonec new <name>            scaffold a new project
quonec build <file.Q>        compile a script to .R
quonec build --script <file> same as above
quonec build --package [dir] compile an R package into <dir>/build/
quonec run <file.Q>          compile, then print Rscript instructions
quonec run --rscript <file>  compile then invoke Rscript on the result
quonec check <file.Q>        typecheck without emitting
quonec deps [dir]            print the auto-derived runtime deps
quonec fmt <file.Q>          format in place      (see FORMATTER.md)
quonec repl                  start an interactive session  (see IDE.md)
quonec lsp                   speak LSP over stdin/stdout   (see IDE.md)
quonec version               print version
quonec --help                print this message
```

Cross-cutting options accepted before or after any subcommand:

```
--diagnostics-format=FMT     human (default) or json (NDJSON)
--out=DIR                    write generated R into DIR
--emit-sourcemap             also emit .R.map sidecars
--rscript                    on `run`, invoke Rscript directly
--log-file=PATH              on `lsp`, mirror traffic to PATH
```

### 3.1 Exit codes

| Code | Meaning                                                       |
| ---- | ------------------------------------------------------------- |
| 0    | success                                                       |
| 1    | one or more diagnostics emitted (parse, type, lex, file load) |
| 2    | usage error (unknown subcommand, missing required argument)   |

### 3.2 `quonec build`

Accepts a `.Q` script in script mode or a directory in `--package`
mode. The lowering rules are normative in
[COMPILATION.md section 1](COMPILATION.md#1-translation-to-r). The
package layout is normative in
[COMPILATION.md section 2.6](COMPILATION.md#26-multi-module-package-layout).

### 3.3 `quonec run`

Compiles the input, then either prints the suggested `Rscript` invocation
(default) or invokes `Rscript` directly when `--rscript` is set.

### 3.4 `quonec check`

Runs the lexer, parser, AST validator, name resolver, and type checker;
emits diagnostics; produces no `.R` output. Exit code 0 means the source
compiles cleanly.

### 3.5 `quonec deps`

Prints the auto-derived runtime dependency set per
[COMPILATION.md section 1.9](COMPILATION.md#19-foreign-r-bindings-and-runtime-dependencies).
Defaults to scanning the current directory; accepts an explicit project
root.

### 3.6 `quonec new`

Scaffolds a new Quone project at the named path with a stock
`quone.toml`, an empty `src/` directory, and a starter `.gitignore`.

### 3.7 `quonec version`

Prints the compiler version string. The version follows
[LANGUAGE.md section 19.8](LANGUAGE.md#198-versioning) (as it stabilises).

<!-- BEGIN tests:a3 -->
**Tests** (11):

- [`cli/no_args_prints_help_section_a3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/CliTests.hs#L49)
- [`cli/version_command_section_a3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/CliTests.hs#L51)
- [`cli/build_defaults_to_script_section_a3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/CliTests.hs#L86)
- [`cli/build_accepts_out_dir_section_a3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/CliTests.hs#L90)
- [`cli/build_package_with_path_section_a3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/CliTests.hs#L104)
- [`cli/build_package_no_path_uses_dot_section_a3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/CliTests.hs#L108)
- [`cli/run_default_no_rscript_section_a3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/CliTests.hs#L117)
- [`cli/run_with_rscript_section_a3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/CliTests.hs#L121)
- [`cli/deps_default_path_is_dot_section_a3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/CliTests.hs#L132)
- [`cli/deps_accepts_path_section_a3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/CliTests.hs#L136)
- [`cli/deps_accepts_json_section_a3`](https://github.com/quone-lang/compiler/blob/19830fa0a68e994f6062b7a0cab2894a70b566d5/tests/Test/CliTests.hs#L140)

<details>
<summary>Show test source</summary>

```haskell
    test "cli/no_args_prints_help_section_a3"
        (Prelude.pure (parseArgs [] === CmdHelp))
    test "cli/version_command_section_a3"
        (Prelude.pure (parseArgs ["version"] === CmdVersion))
    test "cli/build_defaults_to_script_section_a3"
        ( Prelude.pure
            (parseArgs ["build", "src/Foo.Q"]
                === CmdBuildScript defaultBuildOpts "src/Foo.Q"))
    test "cli/build_accepts_out_dir_section_a3"
        ( Prelude.pure
            (parseArgs ["build", "--out=out", "src/Foo.Q"]
                === CmdBuildScript
                    defaultBuildOpts
                        {boOut = Just "out"}
                    "src/Foo.Q"))
    test "cli/build_package_with_path_section_a3"
        ( Prelude.pure
            (parseArgs ["build", "--package", "proj"]
                === CmdBuildPackage defaultBuildOpts "proj"))
    test "cli/build_package_no_path_uses_dot_section_a3"
        ( Prelude.pure
            (parseArgs ["build", "--package"]
                === CmdBuildPackage defaultBuildOpts "."))
    test "cli/run_default_no_rscript_section_a3"
        ( Prelude.pure
            (parseArgs ["run", "src/Foo.Q"]
                === CmdRun defaultRunOpts "src/Foo.Q"))
    test "cli/run_with_rscript_section_a3"
        ( Prelude.pure
            (parseArgs ["run", "--rscript", "src/Foo.Q"]
                === CmdRun
                    defaultRunOpts {roRscript = Prelude.True}
                    "src/Foo.Q"))
    test "cli/deps_default_path_is_dot_section_a3"
        ( Prelude.pure
            (parseArgs ["deps"]
                === CmdDeps HumanDiagnostics "."))
    test "cli/deps_accepts_path_section_a3"
        ( Prelude.pure
            (parseArgs ["deps", "examples/stats-package"]
                === CmdDeps HumanDiagnostics "examples/stats-package"))
    test "cli/deps_accepts_json_section_a3"
        ( Prelude.pure
            (parseArgs
                ["deps", "--diagnostics-format=json", "proj"]
                === CmdDeps JsonDiagnostics "proj"))
```

</details>
<!-- END tests:a3 -->

---

## 4. Subcommands defined in other docs

The remaining subcommands have their normative surface in dedicated
docs because the surface is large enough to deserve its own doc:

| Subcommand     | Defined in                                       |
| -------------- | ------------------------------------------------ |
| `quonec fmt`   | [FORMATTER.md](FORMATTER.md)                      |
| `quonec repl`  | [IDE.md section 2](IDE.md#2-quonec-repl)          |
| `quonec lsp`   | [IDE.md section 1](IDE.md#1-quonec-lsp)           |

---

## See also

- [LANGUAGE.md](LANGUAGE.md) - the Quone language proper.
- [COMPILATION.md](COMPILATION.md) - lowering to R; what `build` produces.
- [FORMATTER.md](FORMATTER.md) - the formatter rules `fmt` enforces.
- [IDE.md](IDE.md) - LSP and REPL surfaces.
