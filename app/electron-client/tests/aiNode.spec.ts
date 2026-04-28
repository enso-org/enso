/**
 * @file End-to-end test for the AI node flow exercising the long-lived `claude` session:
 * one AI prompt, then a manually-typed `Table.input` node, then a second AI prompt that
 * cross-joins both tables. Verifies that (a) multiple requests reuse the same `claude`
 * subprocess and (b) the agent reads the current method's in-scope bindings to pick up
 * nodes that were added between turns.
 *
 * Requires the user-installed `claude` CLI to be on `PATH` and authenticated. CI doesn't
 * have that yet, so this spec is skipped by default — set `ENSO_TEST_AI=1` to enable it.
 */

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

test.skip(
  process.env.ENSO_TEST_AI !== '1',
  'The local `claude` CLI is required; set ENSO_TEST_AI=1 to run this spec.',
)

test('creates two AI nodes plus a manual node in one session', async ({ page }) => {
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

  // 2) First AI prompt with the source node selected. CB stays open (spinner in place of the
  // icon) while the agent runs, closing and committing a new node once generation completes.
  // The agent can take tens of seconds, so bump the timeout well above Playwright's 30s default.
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

  // 6) Visualize the cross-join node. The sentinel is unique to the manually-added Table.input,
  // so its presence in the active visualization proves both rows of the cross product made it
  // through.
  await graphNodes.nth(3).click()
  await visualizeData(page)
  await expect(page.getByText(CROSS_JOIN_SENTINEL)).toBeVisible()
})
