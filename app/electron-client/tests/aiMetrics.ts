/**
 * @file Helpers for collecting AI-effectiveness telemetry from Playwright e2e tests and
 * appending one CSV row per successful test run to a configurable directory.
 */
import type { RequestUsage } from 'enso-common/src/ai'
import { execFile } from 'node:child_process'
import { promises as fs } from 'node:fs'
import path from 'node:path'
import { promisify } from 'node:util'
import type { Page } from 'playwright/test'

const execFileAsync = promisify(execFile)

/**
 * Matches the renderer log line emitted by `logUsage` in
 * `app/gui/src/project-view/composables/ai.ts`. The displayed `context=…k` value is dropped
 * from the regex because it is a lossy kilo-format representation; we recover the exact
 * `contextTokens` sum from the integer triplet `prompt` + `cacheRead` + `cacheCreate`.
 */
const AI_USAGE_LINE_REGEX =
  /\[AI\] usage: prompt=(\d+)t out=(\d+)t context=[\d.]+k \(cacheRead=(\d+)t cacheCreate=(\d+)t\) time=(\d+)ms/

/**
 * Parse one `[AI] usage:` renderer console line into a `RequestUsage`, or `null` if it doesn't
 * match.
 *
 * The line is defined in `logUsage` in `app/gui/src/project-view/composables/ai.ts`.
 */
export function parseAiUsageLine(text: string): RequestUsage | null {
  const m = AI_USAGE_LINE_REGEX.exec(text)
  if (!m) return null
  const inputTokens = Number(m[1])
  const cacheReadTokens = Number(m[3])
  const cacheCreationTokens = Number(m[4])
  return {
    inputTokens,
    outputTokens: Number(m[2]),
    cacheReadTokens,
    cacheCreationTokens,
    contextTokens: inputTokens + cacheReadTokens + cacheCreationTokens,
    durationMs: Number(m[5]),
  }
}

/**
 * Subscribe to the page's renderer console messages and accumulate every `[AI] usage:` line
 * into the returned `samples` array, in the order the renderer logged them. Playwright cleans
 * up the listener when the page closes — no explicit teardown needed.
 */
export function collectAiUsage(page: Page): { samples: RequestUsage[] } {
  const samples: RequestUsage[] = []
  page.on('console', (msg) => {
    const parsed = parseAiUsageLine(msg.text())
    if (parsed) samples.push(parsed)
  })
  return { samples }
}

/**
 * Resolve a tag identifying the source-tree state: the full HEAD SHA when the working tree is
 * clean, or the literal `"WIP"` when `git status --porcelain` produces any output. Treats git
 * failures (no repo, missing binary) as `"WIP"` so a missing-git environment doesn't kill the
 * test on its success path.
 */
export async function gitCommitTag(repoRoot: string): Promise<string> {
  try {
    const [{ stdout: porcelain }, { stdout: sha }] = await Promise.all([
      execFileAsync('git', ['status', '--porcelain'], { cwd: repoRoot }),
      execFileAsync('git', ['rev-parse', 'HEAD'], { cwd: repoRoot }),
    ])
    if (porcelain.trim().length > 0) return 'WIP'
    return sha.trim()
  } catch {
    return 'WIP'
  }
}

/**
 * Produce a filesystem-safe basename for a test title. Lower-cases, collapses any run of
 * non-alphanumeric characters into a single `-`, trims leading/trailing dashes, and appends
 * `.csv`. An all-non-alphanumeric title degrades to `_.csv` so the result is never empty.
 */
export function sanitizeForFilename(testName: string): string {
  const slug = testName
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-+|-+$/g, '')
  return `${slug || '_'}.csv`
}

const CSV_COLUMNS = [
  'timestamp',
  'commit',
  'test_name',
  'node_count',
  'total_duration_ms',
  'total_input_tokens',
  'total_output_tokens',
  'total_cache_read_tokens',
  'total_cache_creation_tokens',
  'final_context_tokens',
  'per_node_durations_ms',
  'per_node_input_tokens',
  'per_node_output_tokens',
  'per_node_cache_read_tokens',
  'per_node_cache_creation_tokens',
  'per_node_context_tokens',
] as const

function csvEscape(value: string): string {
  if (/[",\n\r]/.test(value)) return `"${value.replace(/"/g, '""')}"`
  return value
}

function buildRow(values: readonly string[]): string {
  return values.map(csvEscape).join(',') + '\n'
}

interface AppendMetricsRowArgs {
  /** Directory to write into; created (recursively) if absent. */
  readonly dir: string
  /** Used to derive the CSV filename via {@link sanitizeForFilename} and as the `test_name` column. */
  readonly testName: string
  /** Per-AI-node usage samples in chronological order. Empty arrays are written as zero-count rows. */
  readonly samples: readonly RequestUsage[]
  /** Tag for the `commit` column — typically the result of {@link gitCommitTag}. */
  readonly commit: string
  /** ISO 8601 timestamp for the `timestamp` column. */
  readonly timestamp: string
}

/**
 * Append a single CSV row summarizing one successful test run. Writes the header line first
 * if the target file does not yet exist. Per-node arrays are joined with `;` so the cell
 * never contains a comma; CSV-escaping still runs in case a future column ever does.
 */
export async function appendMetricsRow(args: AppendMetricsRowArgs): Promise<void> {
  await fs.mkdir(args.dir, { recursive: true })
  const csvPath = path.join(args.dir, sanitizeForFilename(args.testName))
  const exists = await fs
    .stat(csvPath)
    .then(() => true)
    .catch(() => false)

  const totalDurationMs = args.samples.reduce((acc, s) => acc + s.durationMs, 0)
  const totalInputTokens = args.samples.reduce((acc, s) => acc + s.inputTokens, 0)
  const totalOutputTokens = args.samples.reduce((acc, s) => acc + s.outputTokens, 0)
  const totalCacheReadTokens = args.samples.reduce((acc, s) => acc + s.cacheReadTokens, 0)
  const totalCacheCreationTokens = args.samples.reduce((acc, s) => acc + s.cacheCreationTokens, 0)
  const finalContextTokens =
    args.samples.length > 0 ? args.samples[args.samples.length - 1]!.contextTokens : 0

  const row = buildRow([
    args.timestamp,
    args.commit,
    args.testName,
    String(args.samples.length),
    String(totalDurationMs),
    String(totalInputTokens),
    String(totalOutputTokens),
    String(totalCacheReadTokens),
    String(totalCacheCreationTokens),
    String(finalContextTokens),
    args.samples.map((s) => s.durationMs).join(';'),
    args.samples.map((s) => s.inputTokens).join(';'),
    args.samples.map((s) => s.outputTokens).join(';'),
    args.samples.map((s) => s.cacheReadTokens).join(';'),
    args.samples.map((s) => s.cacheCreationTokens).join(';'),
    args.samples.map((s) => s.contextTokens).join(';'),
  ])

  const payload = exists ? row : buildRow(CSV_COLUMNS) + row
  await fs.appendFile(csvPath, payload, 'utf8')
}
