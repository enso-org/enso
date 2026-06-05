/**
 * @file Long-running e2e tests that exercise AI-driven analytics workflows through Component
 * Browser AI prompts. Two flavors:
 *
 *   - **Preppin' Data tests (week 32, week 51)** isolate a single capability gap with prompts
 *     that spell out value-dependent context. Week 32 flips green once the agent can read
 *     Standard library `.enso` source files (prompts spell out columns, gym leader order, gym
 *     set tiebreaker, so the only information gap left is "does this stdlib method exist and
 *     what is its signature?"). Week 51 flips green once the agent can fetch the runtime value
 *     of an in-scope binding (step 3 deliberately does NOT specify the Score field's wire
 *     format; the agent must inspect a sample value to figure out the parser).
 *
 *   - **App-demo tests (Colorado COVID, FX Rates History)** describe a business goal and verify
 *     the agent picks the right multi-step approach on its own. Prompts are deliberately
 *     operation-free — they don't name target columns, list joins or aggregations, or dictate
 *     output shape. Assertions key on data-driven invariants (exact row counts where the data
 *     fixes them, sentinel values that must appear regardless of how the agent names columns)
 *     rather than on agent-chosen labels. Each prompt bundles several stdlib operations, so
 *     per-test pass rates are expected to be lower than the Preppin' tests.
 *
 * Each AI prompt creates exactly one new graph node, so an N-step challenge runs ~N AI calls.
 * These tests are SIGNAL, not gates — failures are surfaced honestly with no automatic retries
 * so the developer triaging a run can decide whether the failure is a capability gap or an LLM
 * hiccup.
 *
 * Requires the `claude` CLI on PATH and authenticated (same as `aiNode.spec.ts`), plus
 * `ENSO_TEST_AI_CHALLENGES_DIR=/abs/path` pointing at a directory with the manually-downloaded
 * inputs in the layout below. The spec skips silently when the env var is unset; per-test skips
 * fire when that test's specific files are missing under the path. The FX Rates History test
 * doesn't read any local file (it fetches the BoE CSV over HTTPS at runtime) but still gates on
 * the env var so it doesn't fire in default local runs — any readable directory works.
 *
 * Optional: `ENSO_AI_CHALLENGES_METRICS_DIR=/abs/path` enables effectiveness telemetry. Each
 * run (pass OR fail) appends one CSV row to `<dir>/<sanitized-test-name>.csv` summarizing the
 * run: timestamp, current commit SHA or `WIP` if dirty, the verbatim value of
 * `ENSO_AI_CLAUDE_EXTRA_ARGS` in the `ai_parameters` column (for grouping by model / effort),
 * the `status` (`pass` / `pass (broken)` / `fail` / `fail (broken)`), per-AI-node durations,
 * hop count, the full `usage` breakdown — input / cache_read / cache_creation / output tokens
 * — and the last-hop `contextTokens`. Skipped runs (missing input files) write nothing. See
 * `./aiMetrics.ts` for the schema and the `(broken)` suffix semantics.
 *
 * Expected layout under $ENSO_TEST_AI_CHALLENGES_DIR (flat — files at the top level):
 *   Gym Leader Set Cards.xlsx                         # week 32; sheets: Trainer Cards, Pokemon Cards, Leader Order
 *   Pokemon Input.xlsx                                # week 32; 2 sheets, only `Pokemon` is used
 *   strictly_come_dancing_series_1_to_21_tables.csv   # week 51
 *   CDPHE_COVID19_County_Status_Metrics.csv           # Colorado COVID
 *   ColoradoGeoData.db                                # Colorado COVID; SQLite, `ColoradoLatLong` table
 *
 * The FX Rates History test additionally needs outbound HTTPS to `bankofengland.co.uk`.
 *
 * If a vendor renames a download, edit the corresponding `WEEK_32_FILES` / `WEEK_51_FILES` /
 * `COLORADO_FILES` constant or rename the local copy. For the Preppin' tests the agent
 * identifies which `Data.read` binding is which by reading the sheet name (or file basename)
 * out of the current method's source it receives in its context — so don't rename the
 * sheets/files to opaque labels like `input1`.
 */

import type { RequestUsage } from 'enso-common/src/ai'
import fs from 'node:fs/promises'
import path from 'node:path'
import { expect, type Page, type TestInfo } from 'playwright/test'
import { appendMetricsRow, collectAiUsage, gitCommitTag } from './aiMetrics'
import {
  closeWelcome,
  createNewProject,
  hideVisualization,
  loginAsTestUser,
  test,
  visualizeData,
} from './electronTest'

const DATASETS_DIR = process.env.ENSO_TEST_AI_CHALLENGES_DIR
const METRICS_DIR = process.env.ENSO_AI_CHALLENGES_METRICS_DIR
// 15-min per-prompt ceiling. Stall detection lives in the main process: a turn whose stream-json
// channel falls silent for `IDLE_TIMEOUT_MS` (5 min) errors out and fails this assertion well
// before the per-prompt budget.
const AI_PROMPT_TIMEOUT_MS = 900_000
const MANUAL_NODE_TIMEOUT_MS = 30_000

// `app/electron-client/tests/aiChallenges.spec.ts` → repo root is three `..` up, matching
// the `POSSIBLE_ELECTRON_PATHS` pattern in `electronTest.ts`. Used as the cwd for `git` so
// commit-tag resolution works regardless of where the test runner is invoked from.
const REPO_ROOT = path.resolve(import.meta.dirname, '../../..')

async function recordResult(
  testInfo: TestInfo,
  samples: readonly RequestUsage[],
  outcome: 'pass' | 'fail',
) {
  if (!METRICS_DIR) return
  await appendMetricsRow({
    dir: METRICS_DIR,
    testName: testInfo.title,
    samples,
    commit: await gitCommitTag(REPO_ROOT),
    timestamp: new Date().toISOString(),
    aiParameters: process.env.ENSO_AI_CLAUDE_EXTRA_ARGS ?? '',
    outcome,
  })
}

const WEEK_32_FILES = {
  cardsWorkbook: 'Gym Leader Set Cards.xlsx',
  pokemonInput: 'Pokemon Input.xlsx',
} as const
const WEEK_51_FILES = {
  scores: 'strictly_come_dancing_series_1_to_21_tables.csv',
} as const
const COLORADO_FILES = {
  cases: 'CDPHE_COVID19_County_Status_Metrics.csv',
  geo: 'ColoradoGeoData.db',
} as const

// Fixed Datefrom/Dateto window keeps the dataset bounded and the lenient row-count check stable
// as the calendar moves past Dateto. BoE keeps historical series indefinitely, so the URL stays
// valid; if a series code is renamed at the BoE end, update both the URL and the prompt mapping.
const FX_RATES_URL =
  'https://www.bankofengland.co.uk/boeapps/database/_iadb-fromshowcolumns.asp?csv.x=yes&CSVF=TN&UsingCodes=Y&Datefrom=01/Jan/2020&Dateto=01/Jan/2026&SeriesCodes=XUDLADD,XUDLERD,XUDLNDD,XUDLGBD'

test.skip(
  DATASETS_DIR == null,
  'Set ENSO_TEST_AI_CHALLENGES_DIR to the directory holding AI-challenge inputs. Any readable path satisfies the gate for tests that fetch over HTTPS — see the file docstring for per-test file requirements.',
)

test.use({ aiEnabled: true })

async function resolveDataFiles<T extends Readonly<Record<string, string>>>(
  files: T,
): Promise<{ [K in keyof T]: string }> {
  if (DATASETS_DIR == null) throw new Error('unreachable: module-level skip should have fired')
  const result: Record<string, string> = {}
  const missing: string[] = []
  for (const [key, filename] of Object.entries(files)) {
    const full = path.join(DATASETS_DIR, filename)
    try {
      await fs.access(full, fs.constants.R_OK)
      result[key] = full
    } catch {
      missing.push(full)
    }
  }
  if (missing.length > 0) {
    test.skip(
      true,
      `Missing input files (download from Preppin' Data first):\n  ${missing.join('\n  ')}`,
    )
  }
  return result as { [K in keyof T]: string }
}

/**
 * Wait for the Enso engine to finish evaluating all nodes, then bail if any node is showing an
 * originating Panic, DataflowError, or Missing_Argument. Catching the failure here saves the rest
 * of the AI budget (each downstream prompt would chain off the broken node and either inherit the
 * panic or waste a 5-15s round-trip producing more code on top of bad data).
 *
 * The gate is `.GraphNode.pending`, set by `componentColors.ts` whenever a node's
 * `expressionInfo.payload.type` is `'Unknown'` (no LS payload received yet) or `'Pending'` (engine
 * actively evaluating). It clears only when the engine has produced a final payload (`Value`,
 * `Panic`, or `DataflowError`), so once no node carries `.pending` we know every payload has been
 * received and `useNodeMessage` has had its chance to render the `.GraphNodeMessage`. The
 * neighboring `.GraphNode.evaluating` class isn't sufficient: it's attached only after the first
 * `Pending` update arrives, so a check fired before the engine has even queued a freshly-created
 * node will pass spuriously (the class was never set) and miss the panic that surfaces seconds
 * later — see the AI-component cold-start path where the LS queues evaluation after the placeholder
 * has already been replaced by a real GraphNode.
 *
 * Warnings (`type === 'warning'`) render with the same `.GraphNodeMessage` element when the node
 * is hovered or selected — `useNodeMessage`'s `expand` parameter is `nodeHovered || selected` in
 * `GraphNode.vue`, and our `runAIPromptOnLastNode` selects the new node at the end of each step.
 * They are not pipeline failures, so we discriminate by the SvgIcon's `<use href>`, which is
 * `<iconsUri>#<iconName>` per `svgUseHref` in `app/gui/src/project-view/util/icons.ts`. Per
 * `iconForMessageType` in `GraphNodeMessage.vue` the mapping is:
 *   panic   → `#panic`     (Panic payload — fail)
 *   error   → `#error`     (DataflowError other than Missing_Argument — fail)
 *   missing → `#metadata`  (Missing_Argument DataflowError — fail; AI emitted a call missing args)
 *   warning → `#warning`   (benign — IGNORE)
 */
async function assertNoNodeErrors(page: Page, contextDescription: string) {
  await expect(page.locator('.GraphNode.pending')).toHaveCount(0, { timeout: 60_000 })
  const errorMessages = page.locator(
    '.GraphNodeMessage:has(use[href$="#panic"]), ' +
      '.GraphNodeMessage:has(use[href$="#error"]), ' +
      '.GraphNodeMessage:has(use[href$="#metadata"])',
  )
  const count = await errorMessages.count()
  if (count === 0) return
  const texts = await errorMessages.locator('.message').allInnerTexts()
  throw new Error(
    `Node error after ${contextDescription} — bailing (downstream AI prompts would compound the failure):\n${texts.map((t) => '  - ' + t).join('\n')}`,
  )
}

async function addFreestandingNode(page: Page, expression: string, expectedNodeCount: number) {
  await page.getByTestId('add-component-button').click()
  const cbInput = page.getByTestId('component-editor-content')
  await expect(cbInput).toBeVisible()
  // CB defaults to AI prompt mode when Claude is available (PR #15010). Shift+Enter from aiPrompt
  // drops into component-browsing; the trailing Enter then falls through to code-edit mode (no
  // suggestion matches the literal expression), so the typed code is parsed as code.
  await page.keyboard.press('Shift+Enter')
  await page.keyboard.insertText(expression)
  await page.keyboard.press('Enter')
  await expect(page.locator('.GraphNode')).toHaveCount(expectedNodeCount, {
    timeout: MANUAL_NODE_TIMEOUT_MS,
  })
  await assertNoNodeErrors(page, `creating node \`${expression}\``)
}

/**
 * Close the right-side documentation/activity panel. Its `documentation-editor-content` textbox
 * is rendered with a transparent overlay that extends across part of the graph editor, and
 * Playwright will report that it "intercepts pointer events" if a graph node ends up positioned
 * underneath the panel. With many source nodes spread horizontally, new AI-generated nodes can
 * easily land in that region.
 */
async function closeRightPanel(page: Page) {
  const closeButton = page.getByTestId('right-panel').getByRole('button', { name: 'Close Panel' })
  if (await closeButton.isVisible().catch(() => false)) await closeButton.click()
}

/**
 * Delete the template welcome node that ships with every freshly-created project. Its inline
 * `.TableVisualization` (which `createNewProject` in `electronTest.ts` asserts contains
 * "Welcome To Enso!") otherwise lingers in the DOM for the whole test, so the final-result
 * assertions — which query `.TableVisualization` to inspect the pipeline output — pick up the
 * welcome viz too and either trigger Playwright's strict-mode duplicate-match guard or, worse,
 * silently match the welcome viz instead of the actual output.
 */
async function clearWelcomeNode(page: Page) {
  const graphNodes = page.locator('.GraphNode')
  await expect(graphNodes).toHaveCount(1)
  await graphNodes.first().click()
  await page.keyboard.press('Backspace')
  await expect(graphNodes).toHaveCount(0)
}

async function runAIPromptOnLastNode(page: Page, prompt: string, expectedNodeCount: number) {
  const graphNodes = page.locator('.GraphNode')
  // The currently-selected node is the source for the chained CB; press Enter to open it.
  await page.keyboard.press('Enter')
  const cbInput = page.getByTestId('component-editor-content')
  await expect(cbInput).toBeVisible()
  await page.keyboard.insertText(prompt)
  await page.keyboard.press('Enter')
  await expect(graphNodes).toHaveCount(expectedNodeCount, { timeout: AI_PROMPT_TIMEOUT_MS })
  await assertNoNodeErrors(page, `AI prompt: ${prompt}`)
  // AI-generated nodes are NOT auto-selected after creation (unlike CB-typed nodes — see
  // localWorkflow.spec.ts:120). Without this re-selection, the next iteration's Enter would
  // keep chaining off the original Data.read source, producing parallel branches instead of a
  // pipeline. Click the just-added node (always last in DOM order) to select it for the next
  // chain.
  await graphNodes.last().click()
}

/**
 * Drive an AI prompt without any preceding source node. Used when the prompt itself fetches
 * data (e.g. via `Data.fetch <url>`), so there is no `Data.read` to type by hand first.
 * The CB defaults to AI-prompt mode when Claude is available, so no Shift+Enter dance is needed.
 */
async function runFreestandingAIPrompt(page: Page, prompt: string, expectedNodeCount: number) {
  await page.getByTestId('add-component-button').click()
  const cbInput = page.getByTestId('component-editor-content')
  await expect(cbInput).toBeVisible()
  await page.keyboard.insertText(prompt)
  await page.keyboard.press('Enter')
  const graphNodes = page.locator('.GraphNode')
  await expect(graphNodes).toHaveCount(expectedNodeCount, { timeout: AI_PROMPT_TIMEOUT_MS })
  await assertNoNodeErrors(page, `freestanding AI prompt: ${prompt}`)
  // AI-generated nodes are not auto-selected — see runAIPromptOnLastNode for context.
  await graphNodes.last().click()
}

test("Preppin' Data week 32 — Pokemon Card Organising (stdlib-read isolation)", async ({
  page,
}, testInfo) => {
  // 9 AI calls × up to 15 min each upper bound, plus 4 manual source nodes and the final
  // visualization.
  test.setTimeout(120 * 60_000)
  const files = await resolveDataFiles(WEEK_32_FILES)
  const usage = collectAiUsage(page)
  // Track the outcome explicitly so the `finally` records pass/fail regardless of which
  // assertion threw. `testInfo.status` is not reliable inside a test body (it's set after the
  // test resolves), so a manual flag is the only honest signal here.
  let passed = false
  try {
    await loginAsTestUser(page)
    await closeWelcome(page)
    await createNewProject(page)
    await closeRightPanel(page)
    await clearWelcomeNode(page)

    const graphNodes = page.locator('.GraphNode')

    // Manual: 4 Data.read source nodes — three sheets out of `Gym Leader Set Cards.xlsx` and the
    // Pokemon sheet of `Pokemon Input.xlsx`. The agent identifies which source is which by reading
    // the sheet name out of the method source it receives in its context.
    //
    // Order matters: Pokemon Cards is added last so it ends up auto-selected, and the first AI
    // prompt's chained Component Browser opens off it (the prompt operates on the Card column,
    // which only Pokemon Cards has).
    const sources = [
      `Data.read "${files.pokemonInput}" (..Sheet "Pokemon")`,
      `Data.read "${files.cardsWorkbook}" (..Sheet "Leader Order")`,
      `Data.read "${files.cardsWorkbook}" (..Sheet "Trainer Cards")`,
      `Data.read "${files.cardsWorkbook}" (..Sheet "Pokemon Cards")`,
    ]
    let nodeCount = 0
    for (const expr of sources) {
      nodeCount += 1
      await addFreestandingNode(page, expr, nodeCount)
    }

    // Each prompt spells out any value-dependent context (column names, gym set tiebreaker, …) so
    // this test isolates the "agent knows stdlib" capability — see the file-level note before
    // weakening any prompt. The schemas below mirror the actual sheets:
    //   Pokemon Cards: level, name, number, set_name (where `name` holds e.g. "Brock's Rhydon")
    //   Pokemon Input: Pokédex #, Name, Type, ...
    //   Trainer Cards: Leader, Gym Set, Number, Card
    //   Leader Order:  Order, Leader
    const prompts = [
      'In the Pokemon Cards table (columns: level, name, number, set_name), the `name` column holds' +
        ' apostrophe-s combinations like "Brock\'s Rhydon". Split `name` into two new columns:' +
        ' `Leader` (the part before the apostrophe-s, e.g. "Brock") and `Card` (the part after,' +
        ' e.g. "Rhydon"). Drop the original `name` column; keep level, number, set_name.',
      'Join the result with the Pokemon Input table (columns: Pokédex #, Name, Type, ...) on the' +
        " result's `Card` column matching Pokemon Input's `Name` column, bringing in just the" +
        ' `Pokédex #` column.',
      'Deduplicate the rows by the combination of (Leader, Card, number, set_name).',
      'Rename columns: `set_name` to `Gym Set`, `number` to `Number`, `level` to `Level`. Add a new' +
        ' column `Card Type` with the constant value "Pokémon".',
      'Union with the Trainer Cards table (columns: Leader, Gym Set, Number, Card). For rows coming' +
        ' from Trainer Cards, set `Card Type` to "Trainer".',
      'For rows where `Leader` is null or empty, set `Leader` to "Leftover Trainers".',
      'Join with the Leader Order table (columns: Order, Leader) on the `Leader` column, bringing in' +
        ' `Order` and renaming it to `Sort Order`. For "Leftover Trainers" rows where `Sort Order`' +
        ' is null, set `Sort Order` to 100 so they sort last.',
      'Sort the rows by: Sort Order ascending, then Card Type ascending, then for Trainer rows' +
        " Gym Set with 'Gym Heroes' before 'Gym Challenge', then Number ascending, then" +
        ' `Pokédex #` ascending, then Level ascending.',
      'Project the table to only these columns, in this order: Sort Order, Leader, Gym Set, Number,' +
        ' Card, Card Type.',
    ]
    for (const prompt of prompts) {
      nodeCount += 1
      await runAIPromptOnLastNode(page, prompt, nodeCount)
    }

    await graphNodes.last().click()
    await visualizeData(page)
    for (const col of ['Sort Order', 'Leader', 'Gym Set', 'Number', 'Card', 'Card Type']) {
      // Pick the first `.TableVisualization` — multiple may be rendered at once (inline preview
      // for the selected node plus a focused/fullscreen overlay), and the bare class selector
      // would trip Playwright's strict-mode check.
      await expect(page.locator('.TableVisualization').first()).toContainText(col)
    }
    await expect(page.getByText('Total Row Count: 252')).toBeVisible({
      timeout: MANUAL_NODE_TIMEOUT_MS,
    })
    passed = true
  } finally {
    await recordResult(testInfo, usage.samples, passed ? 'pass' : 'fail')
  }
})

test("Preppin' Data week 51 — Strictly Positive Improvements (value-probe isolation)", async ({
  page,
}, testInfo) => {
  // 6 AI calls × up to 15 min each upper bound, plus the manual source node and the final
  // visualization.
  test.setTimeout(90 * 60_000)
  const files = await resolveDataFiles(WEEK_51_FILES)
  const usage = collectAiUsage(page)
  let passed = false
  try {
    await loginAsTestUser(page)
    await closeWelcome(page)
    await createNewProject(page)
    await closeRightPanel(page)
    await clearWelcomeNode(page)

    const graphNodes = page.locator('.GraphNode')

    await addFreestandingNode(page, `Data.read "${files.scores}"`, 1)

    // Input columns: Series, Week, Couple, Scores, Dance, Music, Result, Film, Broadway musical,
    // Musical, Country, CelebratingBBC. Step 3 deliberately does NOT spell out the `Scores` field's
    // wire format — the agent has to look at a sample value to figure out the parser. See the
    // file-level note.
    const prompts = [
      'drop rows where cell value is actually its column name',
      'convert the `Week` column to a numeric type',
      'parse the `Scores` column into a `total_score` (number) column and a `judges_count` (number)' +
        ' column, then add an `avg_judges_score` column equal to `total_score / judges_count`',
      "keep only each Couple's first dance (a row with their lowest Week) and their final-round rows." +
        ' A final-round row is one where the `Result` column is "Winners", "Runners-up", or "Third place"' +
        ' Drop any couple that never appears in a final-round row.',
      'aggregate the final-round rows per Couple by averaging `avg_judges_score`',
      "compute the percentage change between each Couple's first-dance `avg_judges_score` and" +
        ' their final-round average; project to the columns Series, Couple, Finalist Positions,' +
        " Avg Judge's Score, % Change",
    ]
    let nodeCount = 1
    for (const prompt of prompts) {
      nodeCount += 1
      await runAIPromptOnLastNode(page, prompt, nodeCount)
    }

    await graphNodes.last().click()
    await visualizeData(page)
    for (const col of ['Series', 'Couple', 'Finalist Positions', "Avg Judge's Score", '% Change']) {
      // Pick the first `.TableVisualization` — multiple may be rendered at once (inline preview
      // for the selected node plus a focused/fullscreen overlay), and the bare class selector
      // would trip Playwright's strict-mode check.
      await expect(page.locator('.TableVisualization').first()).toContainText(col)
    }
    await expect(page.getByText('Total Row Count: 62')).toBeVisible({
      timeout: MANUAL_NODE_TIMEOUT_MS,
    })
    passed = true
  } finally {
    await recordResult(testInfo, usage.samples, passed ? 'pass' : 'fail')
  }
})

test('Colorado COVID — cleanup, daily summary, map snapshot', async ({ page }, testInfo) => {
  // 3 AI calls × up to 15 min each upper bound, plus 2 manual source nodes and 2 visualizations.
  test.setTimeout(90 * 60_000)
  const files = await resolveDataFiles(COLORADO_FILES)
  const usage = collectAiUsage(page)
  let passed = false
  try {
    await loginAsTestUser(page)
    await closeWelcome(page)
    await createNewProject(page)
    await closeRightPanel(page)
    await clearWelcomeNode(page)

    const graphNodes = page.locator('.GraphNode')

    // Manual: SQLite source first, then the COVID CSV last so it ends up auto-selected and
    // the first AI prompt chains off it.
    let nodeCount = 0
    for (const expr of [`Data.read "${files.geo}"`, `Data.read "${files.cases}"`]) {
      nodeCount += 1
      await addFreestandingNode(page, expr, nodeCount)
    }

    // Prompt 1: cleanup, chains off CSV. No checkpoint — the daily-summary checkpoint below
    // implicitly verifies the cleanup result is usable.
    nodeCount += 1
    await runAIPromptOnLastNode(
      page,
      'This raw COVID data has more columns than I need. Pare it down to a tidy daily table' +
        ' of cases per county.',
      nodeCount,
    )
    // Index of the cleanup node, used to fork prompt 3 back off it instead of off prompt 2.
    const cleanupNodeIndex = nodeCount - 1

    // Prompt 2: daily summary, chains off prompt 1.
    nodeCount += 1
    await runAIPromptOnLastNode(
      page,
      'Give me a daily summary from the cleaned table — I want to see how cases trended day' +
        ' by day, with the worst-hit county on each day standing out, and a per-county' +
        ' breakdown alongside.',
      nodeCount,
    )

    // Checkpoint: two sentinel county names must appear in the viz (as column headers in a
    // wide breakdown or cell labels in a long one — either passes), and the row count is the
    // exact number of distinct dates in the source.
    await visualizeData(page)
    for (const county of ['OURAY', 'SEDGWICK']) {
      await expect(page.locator('.TableVisualization').first()).toContainText(county)
    }
    await expect(page.getByText('Total Row Count: 1005')).toBeVisible({
      timeout: MANUAL_NODE_TIMEOUT_MS,
    })
    await hideVisualization(page)

    // Re-select the cleanup node so prompt 3 forks off it instead of the daily-summary node.
    await graphNodes.nth(cleanupNodeIndex).click()

    // Prompt 3: map data for a fixed date. The SQLite source name is the only pointer into the
    // geo data the prompt gives — column names inside `ColoradoLatLong` are left for the agent
    // to probe via `mcp__enso__evaluateExpression`.
    nodeCount += 1
    await runAIPromptOnLastNode(
      page,
      'Build a map-ready snapshot for 2022-02-02 from the cleaned data. I want one record per' +
        " county for that single day, with each county's geographic location (the SQLite" +
        ' source has a `ColoradoLatLong` table with center coordinates) and a marker-size' +
        ' value that scales with case count.',
      nodeCount,
    )

    // ag-grid virtualizes rows, so a per-county text sentinel (e.g. `DENVER`) only matches when
    // it happens to land in the first rendered viewport — naming + sort order are both agent
    // choices and not stable across runs. The exact row count (one per Colorado county) is the
    // load-bearing invariant; the daily-summary checkpoint above already exercised cell-text
    // matching via the wide cross-tab format where county names render as column headers (always
    // in the DOM regardless of virtualization).
    await visualizeData(page)
    await expect(page.getByText('Total Row Count: 64')).toBeVisible({
      timeout: MANUAL_NODE_TIMEOUT_MS,
    })
    passed = true
  } finally {
    await recordResult(testInfo, usage.samples, passed ? 'pass' : 'fail')
  }
})

test('FX Rates History — fetch, daily returns, correlation matrix', async ({ page }, testInfo) => {
  // 3 AI calls × up to 15 min each upper bound, plus 2 visualizations. No manual nodes — the
  // first AI prompt fetches the BoE CSV over HTTPS itself.
  test.setTimeout(90 * 60_000)
  const usage = collectAiUsage(page)
  let passed = false
  try {
    await loginAsTestUser(page)
    await closeWelcome(page)
    await createNewProject(page)
    await closeRightPanel(page)
    await clearWelcomeNode(page)

    // Prompt 1: freestanding fetch + name. The XUDL→friendly-name mapping is business knowledge
    // (the agent can't infer "XUDLADD = Australian dollar"), so it stays in the prompt; the
    // operations to perform are not specified.
    await runFreestandingAIPrompt(
      page,
      'Fetch the Bank of England daily exchange-rate CSV at `' +
        FX_RATES_URL +
        '`. The four rate columns there are coded as BoE series IDs — XUDLADD is the' +
        ' Australian dollar, XUDLERD is the Euro, XUDLNDD is the New Zealand dollar, XUDLGBD' +
        ' is Sterling. Give me the table with those currency names instead of the BoE codes so' +
        ' I can use it for downstream analysis.',
      1,
    )

    // Checkpoint: friendly names appear (any "Australian dollar"/"Australian"/etc. substring
    // passes `toContainText('Australian')`); row count is at least 1000 (BoE occasionally
    // adjusts historical series — exact match would be brittle).
    await visualizeData(page)
    for (const name of ['Australian', 'Euro', 'Sterling']) {
      await expect(page.locator('.TableVisualization').first()).toContainText(name)
    }
    const rowCountText = await page
      .getByText(/Total Row Count: \d+/)
      .first()
      .textContent({ timeout: MANUAL_NODE_TIMEOUT_MS })
    const rowCountMatch = rowCountText?.match(/Total Row Count: (\d+)/)
    if (rowCountMatch == null) {
      throw new Error(`Expected "Total Row Count: <N>" in viz, got: ${rowCountText ?? '(empty)'}`)
    }
    expect(Number(rowCountMatch[1])).toBeGreaterThanOrEqual(1000)
    await hideVisualization(page)

    // Prompt 2: daily returns. No checkpoint — prompt 3's correlation matrix is the next gate.
    await runAIPromptOnLastNode(
      page,
      "From the renamed table, compute each currency's daily percentage return.",
      2,
    )

    // Prompt 3: correlation matrix between the four currency-return series.
    await runAIPromptOnLastNode(
      page,
      "Show me how these four currencies' daily returns correlate with each other — the" +
        ' standard pairwise Pearson correlation matrix.',
      3,
    )

    await visualizeData(page)
    for (const name of ['Australian', 'Euro', 'New Zealand', 'Sterling']) {
      await expect(page.locator('.TableVisualization').first()).toContainText(name)
    }
    await expect(page.getByText('Total Row Count: 4')).toBeVisible({
      timeout: MANUAL_NODE_TIMEOUT_MS,
    })
    passed = true
  } finally {
    await recordResult(testInfo, usage.samples, passed ? 'pass' : 'fail')
  }
})
