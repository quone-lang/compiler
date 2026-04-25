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

  context.subscriptions.push(
    vscode.commands.registerCommand("quone.smartEnter", smartEnter)
  );

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

async function smartEnter(): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor || editor.document.languageId !== "quone") {
    await vscode.commands.executeCommand("default:type", { text: "\n" });
    return;
  }

  const document = editor.document;
  const edits = [...editor.selections]
    .sort((a, b) => b.start.compareTo(a.start))
    .map((selection) => {
      const active = selection.active;
      const line = document.lineAt(active.line).text;
      const beforeCursor = line.slice(0, active.character);
      const afterCursor = line.slice(active.character);
      const indent = nextIndent(editor, beforeCursor, afterCursor);
      return { selection, text: `\n${indent}` };
    });

  await editor.edit((builder) => {
    for (const edit of edits) {
      builder.replace(edit.selection, edit.text);
    }
  });
}

function nextIndent(
  editor: vscode.TextEditor,
  beforeCursor: string,
  afterCursor: string
): string {
  if (/^\s*$/.test(beforeCursor)) {
    return "";
  }

  const trimmedBefore = beforeCursor.trim();
  if (/^[}\])]+$/.test(trimmedBefore) && /^\s*$/.test(afterCursor)) {
    return "";
  }

  const currentIndent = beforeCursor.match(/^\s*/)?.[0] ?? "";
  if (/(<-|->|of|in|exporting|then|else)\s*$/.test(beforeCursor)) {
    return currentIndent + indentationUnit(editor);
  }

  return currentIndent;
}

function indentationUnit(editor: vscode.TextEditor): string {
  const options = editor.options;
  if (options.insertSpaces === false) {
    return "\t";
  }

  const tabSize =
    typeof options.tabSize === "number" && options.tabSize > 0
      ? options.tabSize
      : 4;
  return " ".repeat(tabSize);
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}
