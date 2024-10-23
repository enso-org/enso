/** @file Test the search bar and its suggestions. */
import * as test from '@playwright/test'

import * as backend from '#/services/Backend'

import * as actions from './actions'

test.test('tags (positive)', async ({ page }) => {
  await actions.mockAllAndLogin({ page })
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
})

test.test('tags (negative)', async ({ page }) => {
  await actions.mockAllAndLogin({ page })
  const searchBarInput = actions.locateSearchBarInput(page)
  const tags = actions.locateSearchBarTags(page)

  await searchBarInput.click()
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

test.test('labels', async ({ page }) => {
  await actions.mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addLabel('aaaa', backend.COLORS[0])
      api.addLabel('bbbb', backend.COLORS[1])
      api.addLabel('cccc', backend.COLORS[2])
      api.addLabel('dddd', backend.COLORS[3])
    },
  })
  const searchBarInput = actions.locateSearchBarInput(page)
  const labels = actions.locateSearchBarLabels(page)

  await searchBarInput.click()
  for (const label of await labels.all()) {
    const name = (await label.textContent()) ?? ''
    test.expect(name.length).toBeGreaterThan(0)
    await label.click()
    await test.expect(searchBarInput).toHaveValue('label:' + name)
    await label.click()
    await test.expect(searchBarInput).toHaveValue('-label:' + name)
    await label.click()
    await test.expect(searchBarInput).toHaveValue('')
  }
})

test.test('suggestions', async ({ page }) => {
  await actions.mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addDirectory('foo')
      api.addProject('bar')
      api.addSecret('baz')
      api.addSecret('quux')
    },
  })

  const searchBarInput = actions.locateSearchBarInput(page)
  const suggestions = actions.locateSearchBarSuggestions(page)

  await searchBarInput.click()

  for (const suggestion of await suggestions.all()) {
    const name = (await suggestion.textContent()) ?? ''
    test.expect(name.length).toBeGreaterThan(0)
    await suggestion.click()
    await test.expect(searchBarInput).toHaveValue('name:' + name)
    await searchBarInput.selectText()
    await searchBarInput.press('Backspace')
  }
})

test.test('suggestions (keyboard)', async ({ page }) => {
  await actions.mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addDirectory('foo')
      api.addProject('bar')
      api.addSecret('baz')
      api.addSecret('quux')
    },
  })

  const searchBarInput = actions.locateSearchBarInput(page)
  const suggestions = actions.locateSearchBarSuggestions(page)

  await searchBarInput.click()
  for (const suggestion of await suggestions.all()) {
    const name = (await suggestion.textContent()) ?? ''
    test.expect(name.length).toBeGreaterThan(0)
    await page.press('body', 'ArrowDown')
    await test.expect(searchBarInput).toHaveValue('name:' + name)
  }
})

test.test('complex flows', async ({ page }) => {
  const firstName = 'foo'

  await actions.mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addDirectory(firstName)
      api.addProject('bar')
      api.addSecret('baz')
      api.addSecret('quux')
    },
  })
  const searchBarInput = actions.locateSearchBarInput(page)

  await searchBarInput.click()
  await page.press('body', 'ArrowDown')
  await test.expect(searchBarInput).toHaveValue('name:' + firstName)
  await searchBarInput.selectText()
  await searchBarInput.press('Backspace')
  await test.expect(searchBarInput).toHaveValue('')
  await page.press('body', 'ArrowDown')
  await test.expect(searchBarInput).toHaveValue('name:' + firstName)
})
