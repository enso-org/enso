/** @file System and user prompts that orchestrate the AI-Node `claude` subprocess. */

import type { AiComponentRequest } from 'enso-common/src/ai'

/**
 * Priming turn body sent right after spawn. With `stdlibRoot` available, the agent reads
 * three CLAUDE.md files (top-level + Base + Table) and Globs/Reads the Image library before
 * replying `READY`, pre-loading stdlib documentation into context for every subsequent turn.
 * Without a stdlib path there's nothing to ingest, so the prompt collapses to the one-line
 * "say READY" form. See `electron-client/CLAUDE.md`'s "Priming" section for the rationale.
 */
export function buildPrimingPrompt(stdlibRoot: string | undefined): string {
  if (stdlibRoot == null) {
    return 'Acknowledge readiness with the single word READY. This is a session warm-up; do not return JSON.'
  }
  return `This is a session warm-up. Before any user request arrives, study the bundled Enso standard library so you can produce correct code for it.

Step 1. Read these three files entirely, do not skip paragraphs:
- \`${stdlibRoot}/CLAUDE.md\` — universal stdlib conventions, library index, and cross-library pitfalls. Applies to every library; the rules here are not repeated in per-library files.
- \`${stdlibRoot}/Base/0.0.0-dev/CLAUDE.md\` — foundational types and operations. Every other library imports from \`Base\`.
- \`${stdlibRoot}/Table/0.0.0-dev/CLAUDE.md\` — columnar in-memory and database-backed table operations. The most-used non-\`Base\` library.

Other libraries each have a similar \`<Name>/0.0.0-dev/CLAUDE.md\`. **Do not Read those now.** When a later user request needs a library you have not yet loaded, Read that library's \`CLAUDE.md\` then; the content stays in context for the rest of the session.

Step 2. Glob \`Image/0.0.0-dev/src/**/*.enso\` under \`${stdlibRoot}\` and Read every match. The Image library is small (~9 files) and shows idiomatic Enso syntax in a real, complete library — types, methods, conversions, doc blocks.

Once you have finished both steps, reply with a single word: READY. Do not return JSON for this warm-up.`
}

const ENSO_CHEAT_SHEET = `\
Enso is a functional, indentation-sensitive language for data work.
Syntax essentials:
- Assignment: \`name = expression\`. The last expression in a block is its value.
- Method chain: \`table.filter predicate . sort ["date"]\`.
- Lambda: \`x -> x + 1\`.
- Text literals use double quotes: "hello".
- Comments start with \`#\` and are rare in generated code.
- Blocks: indented lines belonging to the same statement.
- Qualified names: \`Standard.Base.Data.Vector.Vector.new\`.
- Constructors: a type holds its constructors. Build a value with \`Type.Constructor args\` (e.g. \`Constant_Column.Value "x"\`) — a bare type name isn't callable unless it has a single constructor sharing its name. The shorthand \`..Constructor args\` (autoscope) only resolves when the surrounding parameter's expected type is a *single* type with that constructor; for parameters typed as a union (\`A | B | C\`) use the qualified \`Type.Constructor\` form.
- Conversions: many wide-input methods accept several source types via \`Target.from (that:Source)\` conversion methods on the target — when the parameter is a union, passing a value of any accepted source type lets the engine coerce it, no wrapper needed.
Common stdlib entry points (Standard.Base / Standard.Table):
- \`Vector.new count fn\`, \`Vector.filter\`, \`Vector.map\`, \`Vector.reduce\`.
- \`Table.filter\`, \`Table.select_columns\`, \`Table.sort\`, \`Table.aggregate\`.
- \`Text.contains\`, \`Text.starts_with\`, \`Text.split\`.
- \`Data.read path\`, \`Data.write path value\`.`

/** Tool capabilities surfaced in the system prompt. `undefined` slots are omitted entirely so the agent isn't told it has tools it can't use. */
export interface SystemPromptTools {
  /** Path passed to `--add-dir`; enables `Read`/`Glob`/`Grep` against the bundled stdlib. */
  readonly stdlibRoot: string | undefined
  /** MCP config path; enables the `evaluateExpression` tool. */
  readonly mcpConfigPath: string | undefined
}

/** Build the agent's `--system-prompt` argument given the tools the child will be launched with. */
export function buildSystemPrompt(tools: SystemPromptTools): string {
  const toolLines: string[] = []
  if (tools.stdlibRoot != null) {
    toolLines.push(
      `- \`Read\`, \`Glob\`, and \`Grep\` against the Enso standard library at \`${tools.stdlibRoot}\`. Prefer reading the actual \`.enso\` source files when in doubt about a function's exact name, signature, or available overloads — your built-in cheat sheet is incomplete. Stay inside that directory; do not attempt to read anything else.`,
      `- The stdlib has a top-level \`${tools.stdlibRoot}/CLAUDE.md\` (universal conventions, library index, cross-library pitfalls) plus a per-library \`${tools.stdlibRoot}/<Name>/0.0.0-dev/CLAUDE.md\` (each library's public API, common usage, pitfalls). You read the top-level file plus \`Base\`'s and \`Table\`'s at session start. **Before using any other library, Read its \`CLAUDE.md\` once first** — it is more compact than the source and quicker than \`Glob\`/\`Grep\` for orientation. The result stays in context for the rest of the session.`,
      `- When unsure about a method's arguments, return type, or usage examples, \`Read\` the source file containing it and consult the \`## \` doc block immediately preceding the definition. Doc blocks list arguments under \`## Arguments\` and runnable examples under \`## Examples\`; they are the authoritative reference, more reliable than guessing from the name alone.`,
      `- Do not call into anything under an \`Internal/\` module path, or any entity whose \`## ---\` metadata block contains \`private: true\`, or any module whose first non-blank source line is \`private\`. These are unstable helpers without API guarantees. Prefer the public API re-exported from each library's \`Main.enso\`.`,
    )
  }
  if (tools.mcpConfigPath != null) {
    toolLines.push(
      '- `evaluateExpression(expression)` — evaluate a plain Enso expression in the same scope your generated `body` would run in. Every in-scope binding listed below is referenceable by name. **The expression must evaluate to Text** (the transport sends raw bytes back as text); pick the encoding yourself with `.to_text`, `.to_display_text`, or `.to_json` depending on what is most useful. For non-Text producers wrap the call: `<binding>.column_names.to_json` (JSON array of column names), `(<binding>.first.to_text).take 200` (preview a value), `(<binding>.row_count).to_text` (a single number), `(cards.join leader_order on=["Set"]).column_names.to_json` (JSON array, schema check). Expressions that may produce a DataflowError need an explicit catch: `((<expr>).catch_primitive (e -> e.to_display_text))` — otherwise the call comes back with the dataflow error wrapped in an actionable hint, not the value you wanted. Each call is a real LS round-trip — pick what you need, don\'t fan out.',
    )
  }
  const toolsSection =
    toolLines.length > 0 ? `\n\nTools you have available:\n${toolLines.join('\n')}` : ''
  return `\
You generate a top-level User Defined Component in Enso — a function definition plus the call that places it inside an existing method on the user's graph.

${ENSO_CHEAT_SHEET}${toolsSection}

You will receive:
- The Enso method the call site lives in (its name and full source).
- The list of \`import\` / \`from … import …\` statements already at the top of the module. Names brought in by these imports are resolvable unqualified; everything else needs a fully qualified name (you cannot add new imports — your only output is the function definition + call). When the surrounding call site is unambiguous about the expected type, the \`..Constructor\` autoscope shorthand avoids long qualified names — see the cheat sheet above for when this applies vs. when you must use \`Type.Constructor\` or rely on a \`Target.from (that:Source)\` conversion.
- Optionally a source binding the user dropped into the AI prompt (identifier and Enso type, when known) — when present, this is the value they want to operate on.
- Other identifiers already in scope in that method, with their Enso types when known. You may reference any of them.
- A natural-language description of what the new component should do.

**Live progress narration (REQUIRED whenever you use any tool).** The user sees these notes as the placeholder node's status text. Every time you start a new logical step that uses tools, emit ONE short text block (≤8 words, present continuous, no code or paths) describing that step in plain English — e.g. "Checking Pokemon Cards columns", "Looking up Table.join signature", "Reading Standard.Table source", "Drafting filter step". One narration covers all the tool calls within that step; you don't need to restate it per tool call. The narration text block must come before the tool_use blocks for that step. If you skip the narration the user just sees "Thinking…" and feels stuck; treat it as part of the contract, not optional. Code, expression text, and file paths must NOT appear in the narration — those are logged separately.

Your closing assistant turn (after the last tool round, or right away if you don't use tools) must be the JSON object described below and nothing else (no narration, no prose, no code fences, no leading or trailing whitespace):
- \`functionName\`: snake_case identifier for the new top-level function. It must not collide with an identifier already used in the surrounding method or with a name visible in the supplied method source. Pick something descriptive of what the function does.
- \`argumentNames\`: parameter names in the function signature, in declaration order. Pick names that describe each parameter's role inside the function — they do *not* have to match any in-scope identifier and they are the names you reference inside \`body\`. Only declare parameters that \`body\` actually uses.
- \`body\`: the function body, as a string. Every line belongs to the body; no leading or trailing blank lines. Reference the parameters by the names you listed in \`argumentNames\`. The final line must be a single identifier — the binding that holds the result. Do not include the function signature, the \`=\` sign, or any module wrapper.
- \`callArguments\`: Enso expressions passed at the call site, one per parameter and in the same order as \`argumentNames\`. Each entry is usually just an in-scope identifier (the source binding or one of the other in-scope bindings), but any single Enso expression is accepted. The renderer wraps them as \`Main.<functionName> <callArguments[0]> <callArguments[1]> ...\`. When a source binding is provided, pass it as a call argument if the function operates on it; pass other in-scope identifiers only when the function uses them. The function may also take no parameters at all if it doesn't depend on anything in scope.

Rules:
- Do not chain method calls on a single line — every line in \`body\` should be at most one outer call so each step shows up as its own graph node. Avoid \`x.foo y . bar z\` and \`x.foo.bar\`; bind the intermediate result to a name and call \`.bar\` on the next line. **Calls inside arguments are fine** — write small constructors and helpers directly as arguments rather than naming intermediates for them, e.g. \`table.filter "age" (..Greater 18)\` is one call, not two (the inner \`..Greater 18\` is an autoscoped \`Filter_Condition\` argument, not a chain).
- The final line of \`body\` must be a single identifier — assign expressions to a name first and reference that name.
- \`argumentNames\` and \`callArguments\` must have the same length.
- Return only valid Enso — avoid placeholders, pseudocode, or commentary.

If the user message is a session warm-up and the request is not for a component, reply briefly in plain text. Otherwise, your closing assistant turn must be the JSON object described above and nothing else.`
}

function formatBinding(identifier: string, typeName: string | undefined): string {
  return typeName ? `- ${identifier} : ${typeName}` : `- ${identifier} : (type unknown)`
}

/** Build the per-turn user prompt from a renderer-issued component request. */
export function buildUserPrompt(request: AiComponentRequest): string {
  const { prompt, context } = request
  const otherBindings = context.inScopeBindings.map((binding) =>
    formatBinding(binding.identifier, binding.typeName),
  )
  const otherBindingsList = otherBindings.length > 0 ? otherBindings.join('\n') : '(none)'
  const moduleImportsList =
    context.moduleImports.length > 0 ? context.moduleImports.join('\n') : '(none)'
  const sourceSection =
    context.sourceIdentifier != null ?
      `Source binding (the value the user wants to operate on):
${formatBinding(context.sourceIdentifier, context.sourceTypeName)}

`
    : ''
  return `Current method: ${context.currentMethodName}
Current method source:
\`\`\`
${context.currentMethodCode}
\`\`\`

Module imports (already in scope without qualification):
${moduleImportsList}

${sourceSection}Other in-scope bindings:
${otherBindingsList}

User request: ${prompt}`
}
