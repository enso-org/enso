/**
 * @file Long-running e2e tests that simulate a user solving Preppin' Data challenges almost
 * entirely through `AI:` prompts (only the `Data.read` source nodes are user-typed).
 *
 *   - week 32 — flips green once the agent can read Standard library `.enso` source files.
 *     Prompts deliberately spell out any value-dependent context (gym leader order, gym set
 *     tiebreaker by name) so the only information gap left is "does this stdlib method exist
 *     and what is its signature?".
 *   - week 51 — flips green once the agent can fetch the runtime value of an in-scope binding.
 *     Step 3 deliberately does NOT specify the Score field's wire format; the agent must inspect
 *     a sample value to figure out the parser.
 *
 * Each AI prompt creates exactly one new graph node, so an N-step challenge runs ~N AI calls.
 * With ~95% per-call success, expected per-test pass rate is ~50-70%. These tests are SIGNAL, not
 * gates — failures are surfaced honestly with no automatic retries so the developer triaging a run
 * can decide whether the failure is a capability gap or an LLM hiccup.
 *
 * Requires the `claude` CLI on PATH and authenticated (same as `aiNode.spec.ts`), plus
 * `ENSO_TEST_AI_CHALLENGES_DIR=/abs/path` pointing at a directory with the manually-downloaded
 * inputs in the layout below. The spec skips silently when the env var is unset; per-test skips
 * fire when that test's specific files are missing under the path.
 *
 * Optional: `ENSO_AI_CHALLENGES_METRICS_DIR=/abs/path` enables effectiveness telemetry. On a
 * successful run the test appends one CSV row to `<dir>/<sanitized-test-name>.csv` summarizing
 * the run (timestamp, current commit SHA or `WIP` if dirty, per-AI-node durations, hop count,
 * the full `usage` breakdown — input / cache_read / cache_creation / output tokens — and the
 * last-hop `contextTokens`). See `./aiMetrics.ts` for the schema.
 *
 * Expected layout under $ENSO_TEST_AI_CHALLENGES_DIR (flat — files at the top level):
 *   Gym Leader Set Cards.xlsx          # week 32; 3 sheets: Trainer Cards, Pokemon Cards, Leader Order
 *   Pokemon Input.xlsx                 # week 32; 2 sheets, only `Pokemon` is used
 *   strictly_come_dancing_series_1_to_21_tables.csv   # week 51
 *
 * If Preppin' Data renames a download, edit `WEEK_32_FILES` / `WEEK_51_FILES` or rename the
 * local copy. The agent identifies which `Data.read` binding is which by reading the sheet name
 * (or file basename) out of the current method's source it receives in its context — so don't
 * rename the sheets/files to opaque labels like `input1`.
 */

import type { RequestUsage } from 'enso-common/src/ai'
import fs from 'node:fs/promises'
import path from 'node:path'
import { expect, type Page, type TestInfo } from 'playwright/test'
import { appendMetricsRow, collectAiUsage, gitCommitTag } from './aiMetrics'
import {
  closeWelcome,
  createNewProject,
  loginAsTestUser,
  test,
  visualizeData,
} from './electronTest'

const DATASETS_DIR = process.env.ENSO_TEST_AI_CHALLENGES_DIR
const METRICS_DIR = process.env.ENSO_AI_CHALLENGES_METRICS_DIR
const AI_PROMPT_TIMEOUT_MS = 240_000
const MANUAL_NODE_TIMEOUT_MS = 30_000

// `app/electron-client/tests/aiChallengePrep.spec.ts` → repo root is three `..` up, matching
// the `POSSIBLE_ELECTRON_PATHS` pattern in `electronTest.ts`. Used as the cwd for `git` so
// commit-tag resolution works regardless of where the test runner is invoked from.
const REPO_ROOT = path.resolve(import.meta.dirname, '../../..')

async function recordSuccess(testInfo: TestInfo, samples: readonly RequestUsage[]) {
  if (!METRICS_DIR) return
  await appendMetricsRow({
    dir: METRICS_DIR,
    testName: testInfo.title,
    samples,
    commit: await gitCommitTag(REPO_ROOT),
    timestamp: new Date().toISOString(),
  })
}

const WEEK_32_FILES = {
  cardsWorkbook: 'Gym Leader Set Cards.xlsx',
  pokemonInput: 'Pokemon Input.xlsx',
} as const
const WEEK_51_FILES = {
  scores: 'strictly_come_dancing_series_1_to_21_tables.csv',
} as const

test.skip(
  DATASETS_DIR == null,
  "Set ENSO_TEST_AI_CHALLENGES_DIR to the directory holding manually-downloaded Preppin' Data inputs.",
)

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
 * Background: `.GraphNode.evaluating` is added while the LS payload type is `Pending` and during
 * the 500ms post-completion visual settle (see `app/gui/src/project-view/components/GraphEditor/
 * GraphNode.vue` `useRecomputation` / `useProgressBackground`). Once `.evaluating` clears across
 * every node, all expression updates have flushed into Vue's reactive layer, so `.GraphNodeMessage`
 * either is or is not present — no race window.
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
  await expect(page.locator('.GraphNode.evaluating')).toHaveCount(0, { timeout: 60_000 })
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
  await page.keyboard.insertText(`AI: ${prompt}`)
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

test("Preppin' Data week 32 — Pokemon Card Organising (stdlib-read isolation)", async ({
  page,
}, testInfo) => {
  // 9 AI calls × up to 240s each, plus 4 manual source nodes and the final visualization. The
  // 3-minute playwright default is far too short; budget 45 min so a hang fails fast without
  // squeezing the worst-case happy path.
  test.setTimeout(45 * 60_000)
  const files = await resolveDataFiles(WEEK_32_FILES)
  const usage = collectAiUsage(page)
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
  await recordSuccess(testInfo, usage.samples)
})

test("Preppin' Data week 51 — Strictly Positive Improvements (value-probe isolation)", async ({
  page,
}, testInfo) => {
  // 6 AI calls × up to 240s each, plus the manual source node and the final visualization.
  test.setTimeout(30 * 60_000)
  const files = await resolveDataFiles(WEEK_51_FILES)
  const usage = collectAiUsage(page)
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
    'drop rows that look like leaked header rows (the data was scraped, so the column-header row' +
      ' repeats throughout the body)',
    'convert the `Week` column to a numeric type and drop rows that fail to parse',
    'parse the `Scores` column into a `total_score` (number) column and a `judges_count` (number)' +
      ' column, then add an `avg_judges_score` column equal to `total_score / judges_count`',
    "keep only each Couple's first dance (their lowest Week) and any of their dances in the" +
      ' final round; restrict to couples who reached the final',
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
  await recordSuccess(testInfo, usage.samples)
})
