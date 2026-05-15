/**
 * @file End-to-end test for the AI node flow exercising the long-lived `claude` session:
 * one AI prompt, then a manually-typed `Table.input` node, then a second AI prompt that
 * cross-joins both tables, then a manual `Data.read` node on a `.csv` file (Enso parses it
 * as a single-column table because the lines hold no commas), then a third AI prompt that
 * splits that column into three numeric columns — the agent has to inspect a real cell value
 * via the `evaluateExpression` MCP tool because the `key: value` layout can't be inferred
 * from the column type alone. Verifies that (a) multiple requests reuse the same `claude`
 * subprocess (with rotation kicking in when the context grows past the configured
 * thresholds), (b) the agent reads the current method's in-scope bindings to pick up nodes
 * that were added between turns, and (c) the renderer-side `useAiToolHandler` subscription
 * survives Component Browser close/reopen cycles so MCP tool calls actually reach the
 * renderer.
 *
 * Requires the user-installed `claude` CLI to be on `PATH` and authenticated. CI doesn't
 * have that yet, so this spec is skipped by default — set `ENSO_TEST_AI=1` to enable it.
 */

import fs from 'node:fs/promises'
import os from 'node:os'
import path from 'node:path'
import { expect } from 'playwright/test'
import { parseAiUsageLine } from './aiMetrics'
import {
  closeWelcome,
  createNewProject,
  loginAsTestUser,
  test,
  visualizeData,
} from './electronTest'

// Force a context-rotation during the test. Defaults (300K soft / 400K hard) won't fire on
// a 3-prompt session, so we set thresholds well below what a single primed turn produces:
// every real prompt then crosses soft, the warming child primes during the next turn or two,
// and the rotation lands before the spec finishes. The assertion at the end confirms it.
// `process.env` is typed read-only here, so set via the index notation that escapes the
// typed `ProcessEnv` declaration (matches how Node-side scripts set env vars elsewhere).
const env = process.env as Record<string, string | undefined>
env.ENSO_AI_SOFT_CONTEXT_THRESHOLD = '5000'
env.ENSO_AI_HARD_CONTEXT_THRESHOLD = '50000'

const FIRST_AI_PROMPT = "count 'a' letters in all cells of the table"
// Deterministic expected output for `FIRST_AI_PROMPT` against `Examples.welcome`. Update this
// alongside the welcome sample if it ever changes.
const EXPECTED_COUNT = '23'
// Unique sentinel that only ever appears in the manually-added `Table.input` node, so the
// final assertion is unambiguous (won't collide with any letter-count output).
const CROSS_JOIN_SENTINEL = 'cross_join_ok'
const TABLE_INPUT_EXPR = `Table.input [['marker', ['${CROSS_JOIN_SENTINEL}']]]`
const SECOND_AI_PROMPT = 'Cross join both tables'

// Quirky `key: value` content with no commas: Enso's CSV auto-detect parses each line as a
// single field, yielding a one-column Table. The column type alone doesn't reveal the
// `key: value` layout — the agent has to call `evaluateExpression` on a sample cell to pick
// a parser.
const DUMMY_TEXT_CONTENT =
  'x: 4 y: 5 size:12\nx:6 y: 7 size: 13\nx:4 y: 5 size:166\nx:13 y: 0124 size: 15\n'
const PARSE_PROMPT = 'split this single column into three numeric columns named x, y, and size'

test.skip(
  process.env.ENSO_TEST_AI !== '1',
  'The local `claude` CLI is required; set ENSO_TEST_AI=1 to run this spec.',
)

test.use({ aiEnabled: true })

test('creates three AI nodes plus two manual nodes in one session', async ({ page }) => {
  // Three AI prompts × up to ~120s each plus setup, manual nodes, and visualizations easily
  // outruns the playwright config's 180s default. Budget 15 min so a single slow AI turn can
  // cover its worst case without prematurely killing the test.
  test.setTimeout(15 * 60_000)
  // Capture renderer-side `Tool called …` lines so the dummy-text step at the end can prove the
  // `useAiToolHandler` subscription is alive after the CB has closed (regression guard for the
  // bug where the handler was scoped to ComponentBrowser and unsubscribed on submit).
  const consoleLines: string[] = []
  page.on('console', (msg) => consoleLines.push(msg.text()))

  await loginAsTestUser(page)
  await closeWelcome(page)
  await createNewProject(page)

  const graphNodes = page.locator('.GraphNode')
  const cbInput = page.getByTestId('component-editor-content')
  // `add-component-button` opens the Component Browser without binding any source node, which
  // is how we drop a freestanding `Table.input` node onto the graph between the two AI prompts.
  const addNewNode = page.getByTestId('add-component-button')

  // 1) Fresh project: exactly one node (the welcome-table source).
  await expect(graphNodes).toHaveCount(1)

  // 2) First AI prompt with the source node selected.
  await graphNodes.first().click()
  await page.keyboard.press('Enter')
  await expect(cbInput).toBeVisible()
  await page.keyboard.type(FIRST_AI_PROMPT)
  await page.keyboard.press('Enter')
  await expect(graphNodes).toHaveCount(2, { timeout: 120_000 })

  // 3) Visualize the first AI node and confirm the deterministic count.
  await graphNodes.nth(1).click()
  await visualizeData(page)
  await expect(page.getByText(EXPECTED_COUNT)).toBeVisible()

  // 4) Add a freestanding `Table.input` node between the two AI prompts. The unique sentinel
  // value lets the final assertion identify the cross-join output unambiguously. The CB defaults
  // to AI mode when claude is available, so we Shift+Enter to drop into component-search mode
  // and have the literal expression committed as code (rather than sent as an AI prompt).
  await addNewNode.click()
  await expect(cbInput).toBeVisible()
  await page.keyboard.press('Shift+Enter')
  await page.keyboard.type(TABLE_INPUT_EXPR)
  await page.keyboard.press('Enter')
  await expect(graphNodes).toHaveCount(3, { timeout: 30_000 })

  // 5) Re-select the original welcome-table source and ask the agent to cross-join. This is
  // the load-bearing step: the agent must read the current method's source — which now contains
  // the manual `Table.input` binding — and thread that binding through the generated function.
  await graphNodes.first().click()
  await page.keyboard.press('Enter')
  await expect(cbInput).toBeVisible()
  await page.keyboard.type(SECOND_AI_PROMPT)
  await page.keyboard.press('Enter')
  await expect(graphNodes).toHaveCount(4, { timeout: 120_000 })

  // 6) Visualize the cross-join node.
  await graphNodes.nth(3).click()
  await visualizeData(page)
  await expect(page.getByText(CROSS_JOIN_SENTINEL)).toBeVisible()

  // 7) Force the agent to use `evaluateExpression`: a `Data.read` source binding on a `.csv`
  // file with no commas, so Enso parses each line as a single field — giving us a one-column
  // Table whose column type doesn't reveal the `key: value` payload. The agent has to peek at
  // a real cell to figure out how to split it. Creating the `Data.read` node manually (rather
  // than asking the agent to construct the read + split in one prompt) trims the agent's work
  // to just the split decision and keeps this step focused on exercising the renderer-side
  // `useAiToolHandler` subscription end-to-end.
  const tmpDir = await fs.mkdtemp(path.join(os.tmpdir(), 'enso-ai-tool-'))
  const dummyPath = path.join(tmpDir, 'dummy.csv')
  await fs.writeFile(dummyPath, DUMMY_TEXT_CONTENT)
  // Manual `Data.read` node — Shift+Enter drops the CB out of the default AI mode into
  // component-search mode so the literal expression commits as code rather than a prompt.
  await addNewNode.click()
  await expect(cbInput).toBeVisible()
  await page.keyboard.press('Shift+Enter')
  await page.keyboard.type(`Data.read "${dummyPath}"`)
  await page.keyboard.press('Enter')
  await expect(graphNodes).toHaveCount(5, { timeout: 30_000 })
  // The newly created node is auto-selected; Enter opens the CB on it as the source for a new
  // AI prompt — no extra click needed (and clicks land in unintended places after the previous
  // step's visualization shifts the canvas).
  await page.keyboard.press('Enter')
  await expect(cbInput).toBeVisible()
  await page.keyboard.type(PARSE_PROMPT)
  await page.keyboard.press('Enter')
  await expect(graphNodes).toHaveCount(6, { timeout: 240_000 })

  // The agent could only have produced a working parser by inspecting a sample value. The
  // renderer logs `[AI tool] called [#…]` for every dispatch from the MCP server (see
  // `aiToolHandler.ts`);
  expect(consoleLines.some((line) => line.startsWith('[AI tool] called ['))).toBe(true)

  // Context-threshold rotation assertion. With env-var thresholds at 5K/50K (set at the top
  // of this file), the soft threshold trips after the first AI turn and the warming agent
  // takes over once it has primed. The session sets `freshAgent=true` on the very first turn
  // run on a newly-promoted primary, and the renderer logs that as the bare ` fresh` keyword
  // in the `[AI] usage:` line. Asserting on that flag is the only robust signal —
  // `contextTokens` alone can grow monotonically with or without rotation when later prompts
  // are heavier than earlier ones (e.g. the dummy-text step does dozens of stdlib reads).
  const samples = consoleLines.flatMap((line) => {
    const parsed = parseAiUsageLine(line)
    return parsed ? [parsed] : []
  })
  expect(
    samples.length,
    '[AI] usage: lines were not captured from the renderer console',
  ).toBeGreaterThanOrEqual(2)
  const freshFlags = samples.map((s) => s.freshAgent === true)
  expect(
    freshFlags.some(Boolean),
    `expected at least one turn to run on a freshly-rotated claude agent; ` +
      `samples flags ${JSON.stringify(freshFlags)}, contexts ${JSON.stringify(samples.map((s) => s.contextTokens))}.`,
  ).toBe(true)
})

test('cancelling a running AI prompt leaves the queue healthy for follow-up prompts', async ({
  page,
}) => {
  // Three AI turns plus a long-running first prompt that's cancelled mid-flight; an extra-fat
  // budget so a slow re-prime after the cancel doesn't time us out.
  test.setTimeout(15 * 60_000)

  await loginAsTestUser(page)
  await closeWelcome(page)
  await createNewProject(page)

  const graphNodes = page.locator('.GraphNode')
  const cbInput = page.getByTestId('component-editor-content')
  const addNewNode = page.getByTestId('add-component-button')
  const placeholder = page.locator('.AiPendingNode')
  const placeholderStatus = placeholder.locator('[data-testid="ai-pending-status"]')

  await expect(graphNodes).toHaveCount(1)

  // 1) Long-running first prompt: ask the agent directly to wait ~2 minutes before responding.
  // The actual sleep length is irrelevant — we cancel as soon as the placeholder flips to
  // "Thinking…", so the only thing that matters is that the turn has reached `started`.
  await addNewNode.click()
  await expect(cbInput).toBeVisible()
  await page.keyboard.type(
    'AI: Wait at least 2 minutes before replying (write a long, slow narration if you must), ' +
      'then return a node that evaluates to 2 + 2.',
  )
  await page.keyboard.press('Enter')

  // 2) Placeholder visible, transitions queued → running.
  await expect(placeholder).toHaveCount(1)
  await expect(placeholderStatus).toHaveText('Thinking…', { timeout: 60_000 })

  // 3) Cancel mid-flight via the placeholder's cancel button (same selector the renderer-side
  // integration tests use; the button has no `data-testid` today). The placeholder clears
  // once the cancel IPC roundtrips through the main process.
  await placeholder.locator('.cancel').click()
  await expect(placeholder).toHaveCount(0, { timeout: 30_000 })
  await expect(graphNodes).toHaveCount(1)

  // 4) Second prompt — a trivial 2 + 2. Must complete and produce a node whose visualization
  // shows 4. This is the regression we're guarding: the cancel must NOT leave the queue
  // wedged or the next turn's child in an unusable state.
  await addNewNode.click()
  await expect(cbInput).toBeVisible()
  await page.keyboard.type('AI: type the literal Enso expression 2 + 2')
  await page.keyboard.press('Enter')
  await expect(graphNodes).toHaveCount(2, { timeout: 180_000 })
  await graphNodes.nth(1).click()
  await visualizeData(page)
  // Use `exact: true` because UI chrome (status-bar clocks, etc.) contains substrings like
  // "14:03" that include the digit 4. The visualization can render the value into more than
  // one widget (JSON link plus table row) — `.first()` is enough to confirm it landed.
  await expect(page.getByText('4', { exact: true }).first()).toBeVisible()

  // 5) Third prompt — confirms a fresh prompt submitted after a cancel-then-recovery cycle
  // also flows through normally.
  await addNewNode.click()
  await expect(cbInput).toBeVisible()
  await page.keyboard.type('AI: type the literal Enso expression 6 * 7')
  await page.keyboard.press('Enter')
  await expect(graphNodes).toHaveCount(3, { timeout: 180_000 })
  await graphNodes.nth(2).click()
  await visualizeData(page)
  await expect(page.getByText('42', { exact: true }).first()).toBeVisible()
})
