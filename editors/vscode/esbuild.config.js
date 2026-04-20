"use strict";

const esbuild = require("esbuild");

const watch = process.argv.includes("--watch");

const baseOptions = {
  entryPoints: ["src/extension.ts"],
  bundle: true,
  platform: "node",
  target: "node18",
  format: "cjs",
  outfile: "dist/extension.js",
  sourcemap: true,
  external: ["vscode"],
  logLevel: "info",
};

async function main() {
  if (watch) {
    const ctx = await esbuild.context(baseOptions);
    await ctx.watch();
    console.log("esbuild: watching for changes...");
  } else {
    await esbuild.build(baseOptions);
  }
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
