/** @file Test the search bar and its suggestions. */
import * as test from '@playwright/test'

import * as backend from '#/services/Backend'

import * as actions from './actions'

test.test('tags', async ({ page }) => {
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
  const { api } = await actions.mockAllAndLogin({ page })
  const searchBarInput = actions.locateSearchBarInput(page)
  const labels = actions.locateSearchBarLabels(page)
  api.addLabel('aaaa', backend.COLORS[0])
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  api.addLabel('bbbb', backend.COLORS[1]!)
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  api.addLabel('cccc', backend.COLORS[2]!)
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  api.addLabel('dddd', backend.COLORS[3]!)
  await actions.login({ page })

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
  const { api } = await actions.mockAllAndLogin({ page })
  const searchBarInput = actions.locateSearchBarInput(page)
  const suggestions = actions.locateSearchBarSuggestions(page)
  api.addDirectory('foo')
  api.addProject('bar')
  api.addSecret('baz')
  api.addSecret('quux')
  await actions.login({ page })

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
  const { api } = await actions.mockAllAndLogin({ page })
  const searchBarInput = actions.locateSearchBarInput(page)
  const suggestions = actions.locateSearchBarSuggestions(page)
  api.addDirectory('foo')
  api.addProject('bar')
  api.addSecret('baz')
  api.addSecret('quux')
  await actions.login({ page })

  await searchBarInput.click()
  for (const suggestion of await suggestions.all()) {
    const name = (await suggestion.textContent()) ?? ''
    test.expect(name.length).toBeGreaterThan(0)
    await page.press('body', 'Tab')
    await test.expect(searchBarInput).toHaveValue('name:' + name)
  }
})

test.test('complex flows', async ({ page }) => {
  const { api } = await actions.mockAllAndLogin({ page })
  const searchBarInput = actions.locateSearchBarInput(page)
  const firstName = 'foo'
  api.addDirectory(firstName)
  api.addProject('bar')
  api.addSecret('baz')
  api.addSecret('quux')
  await actions.login({ page })

  await searchBarInput.click()
  await page.press('body', 'Tab')
  await test.expect(searchBarInput).toHaveValue('name:' + firstName)
  await searchBarInput.selectText()
  await searchBarInput.press('Backspace')
  await test.expect(searchBarInput).toHaveValue('')
  await page.press('body', 'Tab')
  await test.expect(searchBarInput).toHaveValue('name:' + firstName)
})
