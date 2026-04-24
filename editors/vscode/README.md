# Quone for VS Code / Cursor / Positron

Syntax highlighting and Language Server integration for the
[Quone](https://github.com/quone-lang/compiler) language.

## What it provides

- `.Q` file association and syntax highlighting (TextMate grammar in
  `syntaxes/quone.tmLanguage.json`).
- `quonec lsp` Language Server client, giving you:
  - real-time diagnostics
  - hover with inferred types and `#'` doc blocks
  - go-to-definition for top-level bindings
  - in-scope symbol completion
  - format-on-save via `quonec fmt`

## Requirements

The extension drives the `quonec` compiler from the host. Install it
from R:

```r
install.packages("pak")
pak::pak("quone-lang/quone")
quone::install_compiler()
quone::install_lsp()
```

Or download a release binary directly from
[quone-lang/compiler releases](https://github.com/quone-lang/compiler/releases)
and put it on `PATH`.

## Settings

| Setting               | Default   | Description                                     |
| --------------------- | --------- | ----------------------------------------------- |
| `quone.compilerPath`  | `quonec`  | Absolute path to the `quonec` binary.           |
| `quone.trace.server`  | `off`     | Trace LSP messages between the editor and quonec.  |

## Building from source

```sh
cd compiler/editors/vscode
npm install
npm run compile
npm run package    # produces dist/quone-vscode.vsix
code --install-extension dist/quone-vscode.vsix
```

Cursor uses the same extension format:

```sh
cursor --install-extension dist/quone-vscode.vsix
```

You can also run `quone::install_lsp("cursor")` from R. The R helper installs
the extension and writes `quone.compilerPath` into that editor's user settings.

## Working with Cursor and Positron

Cursor and Positron use the same extension format as VS Code. Either install
the published `.vsix` directly or run `quone::install_lsp()` from R and let it
choose the first available editor CLI.

## License

MIT.
