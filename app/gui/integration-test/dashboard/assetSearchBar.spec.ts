/** @file Test the search bar and its suggestions. */
import { expect, test, type Page } from 'integration-test/base'

/** Find a search bar. */
function locateSearchBar(page: Page) {
  // This has no identifying features.
  return page.getByTestId('asset-search-bar')
}

/** Find a list of tags in the search bar. */
function locateSearchBarTags(page: Page) {
  return locateSearchBar(page).getByTestId('asset-search-tag-names').getByRole('button')
}

/** Find a list of labels in the search bar. */
function locateSearchBarLabels(page: Page) {
  return locateSearchBar(page).getByTestId('asset-search-labels').getByRole('button')
}

/** Find a list of labels in the search bar. */
function locateSearchBarSuggestions(page: Page) {
  return locateSearchBar(page).getByTestId('asset-search-suggestion')
}

const FIRST_ASSET_NAME = 'foo'

test('tags (positive)', async ({ drivePage, page }) => {
  await drivePage.goToCategory.cloud().withSearchBar(async (searchBar) => {
    const tags = locateSearchBarTags(page)

    await searchBar.click()
    for (const positiveTag of await tags.all()) {
      await searchBar.selectText()
      await page.keyboard.press('Backspace')
      const text = (await positiveTag.textContent()) ?? ''
      expect(text.length).toBeGreaterThan(0)
      await positiveTag.click()
      await expect(searchBar).toHaveValue(text)
    }
  })
})

test('tags (negative)', async ({ drivePage, page }) => {
  await drivePage.goToCategory.cloud().withSearchBar(async (searchBar) => {
    const tags = locateSearchBarTags(page)

    await searchBar.click()
    await page.keyboard.down('Shift')
    for (const negativeTag of await tags.all()) {
      await searchBar.selectText()
      await searchBar.press('Backspace')
      const text = (await negativeTag.textContent()) ?? ''
      expect(text.length).toBeGreaterThan(0)
      await negativeTag.click()
      await expect(searchBar).toHaveValue(text)
    }
  })
})

test.skip('labels (were supported in list directory, but not supported in search)', async ({
  drivePage,
  page,
  cloudApi,
}) => {
  cloudApi.addLabel('aaaa', { lightness: 50, chroma: 66, hue: 7 })
  cloudApi.addLabel('bbbb', { lightness: 50, chroma: 66, hue: 34 })
  cloudApi.addLabel('cccc', { lightness: 50, chroma: 66, hue: 80 })
  cloudApi.addLabel('dddd', { lightness: 50, chroma: 66, hue: 139 })

  await drivePage.goToCategory.cloud().withSearchBar(async (searchBar) => {
    const labels = locateSearchBarLabels(page)

    await searchBar.click()
    for (const label of await labels.all()) {
      const name = (await label.textContent()) ?? ''
      expect(name.length).toBeGreaterThan(0)
      await label.click()
      await expect(searchBar).toHaveValue('label:' + name)
      await label.click()
      await expect(searchBar).toHaveValue('')
    }
  })
})

test('suggestions', async ({ drivePage, page, cloudApi }) => {
  cloudApi.addDirectory({ title: 'foo' })
  cloudApi.addProject({ title: 'bar' })
  cloudApi.addSecret({ title: 'baz' })
  cloudApi.addSecret({ title: 'quux' })
  await drivePage.goToCategory.cloud().withSearchBar(async (searchBar) => {
    const suggestions = locateSearchBarSuggestions(page)

    await searchBar.click()

    for (const suggestion of await suggestions.all()) {
      const name = (await suggestion.textContent()) ?? ''
      expect(name.length).toBeGreaterThan(0)
      await suggestion.click()
      await expect(searchBar).toHaveValue('name:' + name)
      await searchBar.selectText()
      await searchBar.press('Backspace')
    }
  })
})

test('suggestions (keyboard)', async ({ drivePage, page, cloudApi }) => {
  cloudApi.addDirectory({ title: 'foo' })
  cloudApi.addProject({ title: 'bar' })
  cloudApi.addSecret({ title: 'baz' })
  cloudApi.addSecret({ title: 'quux' })

  await drivePage.goToCategory.cloud().withSearchBar(async (searchBar) => {
    const suggestions = locateSearchBarSuggestions(page)

    await searchBar.click()
    for (const suggestion of await suggestions.all()) {
      const name = (await suggestion.textContent()) ?? ''
      expect(name.length).toBeGreaterThan(0)
      await page.keyboard.press('ArrowDown')
      await expect(searchBar).toHaveValue('name:' + name)
    }
  })
})

test('complex flows', async ({ drivePage, page, cloudApi }) => {
  cloudApi.addDirectory({ title: FIRST_ASSET_NAME })
  cloudApi.addProject({ title: 'bar' })
  cloudApi.addSecret({ title: 'baz' })
  cloudApi.addSecret({ title: 'quux' })
  await drivePage.goToCategory.cloud().withSearchBar(async (searchBar) => {
    await searchBar.click()
    await page.keyboard.press('ArrowDown')
    await expect(searchBar).toHaveValue('name:' + FIRST_ASSET_NAME)
    await searchBar.selectText()
    await searchBar.press('Backspace')
    await expect(searchBar).toHaveValue('')
    await page.keyboard.press('ArrowDown')
    await expect(searchBar).toHaveValue('name:' + FIRST_ASSET_NAME)
  })
})
