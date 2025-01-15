import type { Page } from '@playwright/test'
import * as lsTypes from 'ydoc-shared/languageServerTypes/suggestions'

/** Add an entry to the suggestion database. */
export async function mockSuggestion(page: Page, update: lsTypes.SuggestionEntry) {
  await page.evaluate(({ update }) => (window as any)._mockSuggestion(update), { update })
}
