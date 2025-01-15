/** @file Test the search bar and its suggestions. */
import { expect, test, type Page } from '@playwright/test'

import { COLORS } from '#/services/Backend'

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

test('tags (negative)', ({ page }) =>
  mockAllAndLogin({ page }).withSearchBar(async (searchBar) => {
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
  }))

test('labels', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addLabel('aaaa', COLORS[0])
      api.addLabel('bbbb', COLORS[1])
      api.addLabel('cccc', COLORS[2])
      api.addLabel('dddd', COLORS[3])
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
      await expect(searchBar).toHaveValue('-label:' + name)
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
