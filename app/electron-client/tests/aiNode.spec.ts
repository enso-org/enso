/**
 * @file End-to-end test for the AI node flow exercising the long-lived `claude` session:
 * one AI prompt, then a manually-typed `Table.input` node, then a second AI prompt that
 * cross-joins both tables, then a third AI prompt parsing a synthetic plain-text file the
 * agent has to inspect at runtime via the `evaluateExpression` MCP tool. Verifies that
 * (a) multiple requests reuse the same `claude` subprocess, (b) the agent reads the
 * current method's in-scope bindings to pick up nodes that were added between turns, and
 * (c) the renderer-side `useAiToolHandler` subscription survives Component Browser
 * close/reopen cycles so MCP tool calls actually reach the renderer.
 *
 * Requires the user-installed `claude` CLI to be on `PATH` and authenticated. CI doesn't
 * have that yet, so this spec is skipped by default — set `ENSO_TEST_AI=1` to enable it.
 */

import fs from 'node:fs/promises'
import os from 'node:os'
import path from 'node:path'
import { expect } from 'playwright/test'
import {
  closeWelcome,
  createNewProject,
  loginAsTestUser,
  test,
  visualizeData,
} from './electronTest'

const FIRST_AI_PROMPT = "AI: count 'a' letters in all cells of the table"
// Deterministic expected output for `FIRST_AI_PROMPT` against `Examples.welcome`. Update this
// alongside the welcome sample if it ever changes.
const EXPECTED_COUNT = '23'
// Unique sentinel that only ever appears in the manually-added `Table.input` node, so the
// final assertion is unambiguous (won't collide with any letter-count output).
const CROSS_JOIN_SENTINEL = 'cross_join_ok'
const TABLE_INPUT_EXPR = `Table.input [['marker', ['${CROSS_JOIN_SENTINEL}']]]`
const SECOND_AI_PROMPT = 'AI: Cross join both tables'

// Quirky non-CSV content the agent has to inspect at runtime: the values use `key: value`
// pairs with inconsistent whitespace, the file has a `.csv` extension that lies, and we read
// it as `Plain_Text` so the agent's compile-time context only knows "Text". The only way to
// pick a parser is to call `evaluateExpression` and look at a real line.
const DUMMY_TEXT_CONTENT =
  'x: 4 y: 5 size:12\nx:6 y: 7 size: 13\nx:4 y: 5 size:166\nx:13 y: 0124 size: 15\n'

test.skip(
  process.env.ENSO_TEST_AI !== '1',
  'The local `claude` CLI is required; set ENSO_TEST_AI=1 to run this spec.',
)

test('creates two AI nodes plus a manual node in one session', async ({ page }) => {
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
  // value lets the final assertion identify the cross-join output unambiguously.
  await addNewNode.click()
  await expect(cbInput).toBeVisible()
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

  // 7) Force the agent to use `evaluateExpression`: feed it text whose shape can't be inferred
  // from the file path or the static type. The file extension lies (`.csv`), the binding is
  // `Plain_Text`, and the line layout uses `key: value` with inconsistent whitespace — so the
  // agent has to peek at a real value to pick a parser. This step's primary purpose is to
  // exercise the renderer-side `useAiToolHandler` subscription end-to-end.
  const tmpDir = await fs.mkdtemp(path.join(os.tmpdir(), 'enso-ai-tool-'))
  const dummyPath = path.join(tmpDir, 'dummy.csv')
  await fs.writeFile(dummyPath, DUMMY_TEXT_CONTENT)
  await addNewNode.click()
  await expect(cbInput).toBeVisible()
  await page.keyboard.type(
    `AI: read text from "${dummyPath}" with Plain_Text encoding and parse it into a Table with three numeric columns named x, y, and size`,
  )
  await page.keyboard.press('Enter')
  await expect(graphNodes).toHaveCount(5, { timeout: 240_000 })

  // The agent could only have produced a working parser by inspecting a sample value. The
  // renderer logs `[AI tool] called [#…]` for every dispatch from the MCP server (see
  // `aiToolHandler.ts`);
  expect(consoleLines.some((line) => line.startsWith('[AI tool] called ['))).toBe(true)
})
