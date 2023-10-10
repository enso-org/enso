/** @file Test GPU settings. */
// chrome://gpu/

import * as test from '@playwright/test'

test.test('gpu', async ({ page }) => {
    await page.goto('chrome://gpu')
    // eslint-disable-next-line @typescript-eslint/no-magic-numbers
    await page.waitForTimeout(1000)
    // eslint-disable-next-line no-restricted-properties
    const html = await page
        .locator('info-view')
        .first()
        .evaluate(node => node.shadowRoot?.innerHTML)
    const formatted = html?.replace(/<\/[^>]*>/g, '$&\n')
    test.expect(formatted).toBe('')
})
