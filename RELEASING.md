# Releasing the Quone compiler

This document is the canonical source for how `quonec` releases are
named, built, and published. The R companion package
([quone-lang/quone](https://github.com/quone-lang/quone)) and any
external installer scripts MUST follow the conventions below, since
they download release artifacts by exact name.

## Versioning

`quonec` follows [Semantic Versioning 2.0.0](https://semver.org/).
Release tags are `vMAJOR.MINOR.PATCH`, e.g. `v0.0.1`. The `Cabal`
package version (`compiler.cabal` `version:` field) MUST match the
git tag with the leading `v` stripped.

## Asset naming

For each tagged release, the GitHub Releases page MUST publish one
prebuilt binary per supported `(os, arch)` pair, packaged as a
gzipped tarball named:

```
quonec-<os>-<arch>.tar.gz
```

with:

| field | allowed values                  |
| ----- | ------------------------------- |
| `os`  | `macos`, `linux`                |
| `arch`| `x86_64`, `arm64`               |

The supported `(os, arch)` pairs for initial release are:

- `quonec-macos-arm64.tar.gz`
- `quonec-linux-x86_64.tar.gz`
- `quonec-linux-arm64.tar.gz`

Each tarball MUST contain a single binary named `quonec` at the archive root,
marked executable (`chmod +x`).

The VS Code-compatible extension MUST be published as:

```
quone-vscode.vsix
```

This stable name lets `quone::install_lsp(version = "latest")` resolve the
current editor extension without first discovering the latest version number.

## Latest pointer

`https://github.com/quone-lang/compiler/releases/latest/download/<asset>`
MUST resolve to the asset for the most recent non-prerelease tag.
This is what `quone::install_compiler(version = "latest")` reads
([quone/R/compiler.R `release_asset_url`](../quone/R/compiler.R)).

## Version-specific URL

`https://github.com/quone-lang/compiler/releases/download/<tag>/<asset>`
MUST resolve to the asset for the named tag. This is what
`quone::install_compiler(version = "0.0.1")` reads.

## Release procedure

1. Bump `version:` in [compiler.cabal](compiler.cabal).
2. Run the release validation in CI.
3. `git tag vMAJOR.MINOR.PATCH && git push --tags`.
4. The `release.yaml` GitHub Actions workflow under
   [.github/workflows/](./.github/workflows/) builds binaries for
   every supported `(os, arch)` and uploads them to the GitHub
   Release with the names above.
5. Verify `https://github.com/quone-lang/compiler/releases/latest/download/quonec-linux-x86_64.tar.gz`
   resolves before announcing the release.
6. Update [`quone-lang/quone`](https://github.com/quone-lang/quone)'s
   `DESCRIPTION` if a feature you ship needs the new compiler version.

## Verification at install time

`quone::install_compiler()` does not currently verify SHA256 checksums. For the
initial release, integrity is taken from GitHub's HTTPS transport.

## Out of scope for initial release

- Statically-linked Linux musl builds.
- macOS x86_64 builds. GitHub-hosted Intel macOS runners are not reliable enough
  for the initial release pipeline.
- Windows builds. The current dependency tree contains filenames that cannot be
  checked out on Windows runners.
- 32-bit targets.
- Homebrew / Scoop / Winget formulae.
- Signed Apple notarised binaries.
