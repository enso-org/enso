/** @file Functions related to the keyboard. */
import type * as test from 'playwright/test'

/** Return `Meta` (`Cmd`) on macOS, and `Control` on all other systems. */
export async function modKey(page: test.Page) {
    const userAgent = await page.evaluate(() => navigator.userAgent)
    return /macOS/.test(userAgent) ? 'Meta' : 'Control'
}
