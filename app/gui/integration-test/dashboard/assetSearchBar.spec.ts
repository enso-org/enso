/** @file Test the search bar and its suggestions. */
import { expect, test, type Page } from 'playwright/test'
import { mockAllAndLogin } from './actions'

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

test('tags (positive)', ({ page }) =>
  mockAllAndLogin({ page }).withSearchBar(async (searchBarInput) => {
    const tags = locateSearchBarTags(page)

    await searchBarInput.click()
    for (const positiveTag of await tags.all()) {
      await searchBarInput.selectText()
      await searchBarInput.press('Backspace')
      const text = (await positiveTag.textContent()) ?? ''
      expect(text.length).toBeGreaterThan(0)
      await positiveTag.click()
      await expect(searchBarInput).toHaveValue(text)
    }
  }))

test.skip('labels (were supported in list directory, but not supported in search)', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addLabel('aaaa', { lightness: 50, chroma: 66, hue: 7 })
      api.addLabel('bbbb', { lightness: 50, chroma: 66, hue: 34 })
      api.addLabel('cccc', { lightness: 50, chroma: 66, hue: 80 })
      api.addLabel('dddd', { lightness: 50, chroma: 66, hue: 139 })
    },
  }).withSearchBar(async (searchBar) => {
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
  }))

test('suggestions', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addDirectory({ title: 'foo' })
      api.addProject({ title: 'bar' })
      api.addSecret({ title: 'baz' })
      api.addSecret({ title: 'quux' })
    },
  }).withSearchBar(async (searchBar) => {
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
  }))

test('suggestions (keyboard)', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addDirectory({ title: 'foo' })
      api.addProject({ title: 'bar' })
      api.addSecret({ title: 'baz' })
      api.addSecret({ title: 'quux' })
    },
  }).withSearchBar(async (searchBar) => {
    const suggestions = locateSearchBarSuggestions(page)

    await searchBar.click()
    for (const suggestion of await suggestions.all()) {
      const name = (await suggestion.textContent()) ?? ''
      expect(name.length).toBeGreaterThan(0)
      await page.press('body', 'ArrowDown')
      await expect(searchBar).toHaveValue('name:' + name)
    }
  }))

test('complex flows', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addDirectory({ title: FIRST_ASSET_NAME })
      api.addProject({ title: 'bar' })
      api.addSecret({ title: 'baz' })
      api.addSecret({ title: 'quux' })
    },
  }).withSearchBar(async (searchBar) => {
    await searchBar.click()
    await page.press('body', 'ArrowDown')
    await expect(searchBar).toHaveValue('name:' + FIRST_ASSET_NAME)
    await searchBar.selectText()
    await searchBar.press('Backspace')
    await expect(searchBar).toHaveValue('')
    await page.press('body', 'ArrowDown')
    await expect(searchBar).toHaveValue('name:' + FIRST_ASSET_NAME)
  }))
