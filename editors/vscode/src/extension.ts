import * as vscode from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

export function activate(context: vscode.ExtensionContext): void {
  const config = vscode.workspace.getConfiguration("quone");
  const compilerPath = config.get<string>("compilerPath", "quonec");

  const serverOptions: ServerOptions = {
    command: compilerPath,
    args: ["lsp"],
    transport: TransportKind.stdio,
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: "file", language: "quone" },
      { scheme: "untitled", language: "quone" },
    ],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher("**/*.Q"),
    },
  };

  client = new LanguageClient(
    "quonec",
    "Quone Language Server",
    serverOptions,
    clientOptions
  );

  context.subscriptions.push({
    dispose: () => {
      void client?.stop();
    },
  });

  client.start().catch((err: unknown) => {
    void vscode.window.showErrorMessage(
      `Failed to start Quone language server (${compilerPath} lsp): ${
        err instanceof Error ? err.message : String(err)
      }. Run \`quone::install_compiler()\` from R or set quone.compilerPath.`
    );
  });
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}
