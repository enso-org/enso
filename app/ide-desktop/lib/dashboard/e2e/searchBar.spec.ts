/** @file Test the search bar and its suggestions. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test.beforeEach(actions.mockAllAndLogin)

test.test('tags', async ({ page }) => {
    const searchBarInput = actions.locateSearchBarInput(page)
    const tags = actions.locateSearchBarTags(page)

    await searchBarInput.click()
    for (const positiveTag of await tags.all()) {
        await searchBarInput.selectText()
        await searchBarInput.press('Backspace')
        const text = (await positiveTag.textContent()) ?? ''
        test.expect(text.length).toBeGreaterThan(0)
        await positiveTag.click()
        await test.expect(searchBarInput).toHaveValue(text)
    }

    await page.keyboard.down('Shift')
    for (const negativeTag of await tags.all()) {
        await searchBarInput.selectText()
        await searchBarInput.press('Backspace')
        const text = (await negativeTag.textContent()) ?? ''
        test.expect(text.length).toBeGreaterThan(0)
        await negativeTag.click()
        await test.expect(searchBarInput).toHaveValue(text)
    }
})
