/**
 * @file End-to-end test for the AI node flow: select the default source node, type an
 * `AI:` prompt into the Component Browser, confirm that a new AI-generated node lands on
 * the graph, and verify the computed value via the default visualization.
 *
 * Requires the user-installed `claude` CLI to be on `PATH` and authenticated. CI doesn't
 * have that yet, so this spec is skipped by default — set `ENSO_TEST_AI=1` to enable it
 * (the plan lists this as the standard per-step verification).
 */

import { expect } from 'playwright/test'
import {
  closeWelcome,
  createNewProject,
  loginAsTestUser,
  test,
  visualizeData,
} from './electronTest'

const AI_PROMPT = "AI: count 'a' letters in all cells of the table"
// Deterministic expected output for `AI_PROMPT` against `Examples.welcome`. Update this
// alongside the welcome sample if it ever changes.
const EXPECTED_COUNT = '23'

test.skip(
  process.env.ENSO_TEST_AI !== '1',
  'The local `claude` CLI is required; set ENSO_TEST_AI=1 to run this spec.',
)

test('creates a User Defined Component from an AI prompt', async ({ page }) => {
  await loginAsTestUser(page)
  await closeWelcome(page)
  await createNewProject(page)

  // The freshly created project has exactly one node — the welcome-table source.
  const graphNodes = page.locator('.GraphNode')
  await expect(graphNodes).toHaveCount(1)
  await graphNodes.first().click()

  // Enter opens the Component Browser with the selected node bound as source.
  await page.keyboard.press('Enter')
  const cbInput = page.getByTestId('component-editor-content')
  await expect(cbInput).toBeVisible()
  await page.keyboard.type(AI_PROMPT)

  // Submit. CB stays open (spinner in place of the icon) while the agent runs, closing
  // and committing a new node once generation completes. The agent can take tens of
  // seconds, so bump the expect timeout well above Playwright's 30s default.
  await page.keyboard.press('Enter')
  await expect(graphNodes).toHaveCount(2, { timeout: 120_000 })

  // Verify the whole package round-trip, not just that a node exists: the new node's
  // default visualization should render the deterministic count for this prompt.
  await graphNodes.nth(1).click()
  await visualizeData(page)
  await expect(page.getByText(EXPECTED_COUNT)).toBeVisible()
})
