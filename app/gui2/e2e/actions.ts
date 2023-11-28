import { type Page } from '@playwright/test'

// =================
// === goToGraph ===
// =================

/** Perform a successful login. */
export async function goToGraph(page: Page) {
  await page.goto('/')
  // Originally this clicked the play button but for now that is out of scope.
}
