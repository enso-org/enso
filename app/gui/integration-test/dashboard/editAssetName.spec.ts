/** @file Test copying, moving, cutting and pasting. */
import { expect, test, type Locator, type Page } from '@playwright/test'

import { TEXT, getText, mockAllAndLogin } from './actions'

const NEW_NAME = 'foo bar baz'
const NEW_NAME_2 = 'foo bar baz quux'

/** Find the context menu. */
function locateContextMenu(page: Page) {
  // This has no identifying features.
  return page.getByTestId('context-menu')
}

/** Find the name column of the given assets table row. */
function locateAssetRowName(locator: Locator) {
  return locator.getByTestId('asset-row-name')
}

function locateInput(nameLocator: Locator) {
  return nameLocator.getByRole('textbox')
}

/** Find a tick button. */
function locateEditingTick(page: Locator) {
  return page.getByLabel(TEXT.confirmEdit)
}

/** Find a cross button. */
function locateEditingCross(page: Locator) {
  return page.getByLabel(TEXT.cancelEdit)
}

test('edit name (double click)', ({ page }) =>
  mockAllAndLogin({ page })
    .createFolder()
    .driveTable.withRows(async (rows, _, { api }) => {
      const row = rows.nth(0)
      const nameEl = locateAssetRowName(row)
      await nameEl.click()
      await nameEl.click()
      await locateInput(nameEl).fill(NEW_NAME)
      const calls = api.trackCalls()
      await locateEditingTick(row).click()
      await expect(row).toHaveText(new RegExp('^' + NEW_NAME))
      expect(calls.updateDirectory).toMatchObject([{ title: NEW_NAME }])
    }))

test('edit name (context menu)', ({ page }) =>
  mockAllAndLogin({ page })
    .createFolder()
    .driveTable.withRows(async (rows, _, { api }) => {
      const row = rows.nth(0)
      await locateAssetRowName(row).click({ button: 'right' })
      await locateContextMenu(page)
        .getByText(/Rename/)
        .click()
      const nameEl = locateAssetRowName(row)
      await expect(locateInput(nameEl)).toBeVisible()
      await expect(locateInput(nameEl)).toBeFocused()
      await locateInput(nameEl).fill(NEW_NAME)
      await expect(locateInput(nameEl)).toHaveValue(NEW_NAME)
      const calls = api.trackCalls()
      await nameEl.press('Enter')
      await expect(row).toHaveText(new RegExp('^' + NEW_NAME))
      expect(calls.updateDirectory).toMatchObject([{ title: NEW_NAME }])
    }))

test('edit name (keyboard)', ({ page }) =>
  mockAllAndLogin({ page })
    .createFolder()
    .driveTable.withRows(async (rows) => {
      await locateAssetRowName(rows.nth(0)).click()
    })
    .press('Mod+R')
    .driveTable.withRows(async (rows, _, { api }) => {
      const row = rows.nth(0)
      const nameEl = locateAssetRowName(row)
      await locateInput(nameEl).fill(NEW_NAME_2)
      const calls = api.trackCalls()
      await nameEl.press('Enter')
      await expect(row).toHaveText(new RegExp('^' + NEW_NAME_2))
      expect(calls.updateDirectory).toMatchObject([{ title: NEW_NAME_2 }])
    }))

test('cancel editing name (double click)', ({ page }) =>
  mockAllAndLogin({ page })
    .createFolder()
    .driveTable.withRows(async (rows, _, { api }) => {
      const row = rows.nth(0)
      const nameEl = locateAssetRowName(row)
      const oldName = (await nameEl.textContent()) ?? ''
      await nameEl.click()
      await nameEl.click()
      await nameEl.getByTestId('input').fill(NEW_NAME)
      const calls = api.trackCalls()
      await locateEditingCross(row).click()
      await expect(row).toHaveText(new RegExp('^' + oldName))
      expect(calls.updateDirectory).toMatchObject([])
    }))

test('cancel editing name (keyboard)', ({ page }) => {
  let oldName = ''
  return mockAllAndLogin({ page })
    .createFolder()
    .driveTable.withRows(async (rows) => {
      await rows.nth(0).click()
    })
    .press('Mod+R')
    .driveTable.withRows(async (rows, _, { api }) => {
      const row = rows.nth(0)
      const nameEl = locateAssetRowName(row)
      oldName = (await nameEl.textContent()) ?? ''
      await nameEl.getByTestId('input').fill(NEW_NAME_2)
      const calls = api.trackCalls()
      await nameEl.press('Escape')
      await expect(row).toHaveText(new RegExp('^' + oldName))
      expect(calls.updateDirectory).toMatchObject([])
    })
})

test('change to blank name (double click)', ({ page }) =>
  mockAllAndLogin({ page })
    .createFolder()
    .driveTable.withRows(async (rows, _, { api }) => {
      const row = rows.nth(0)
      const nameEl = locateAssetRowName(row)
      const oldName = (await nameEl.textContent()) ?? ''
      await nameEl.click()
      await nameEl.click()
      await nameEl.getByTestId('input').fill('')
      await expect(locateEditingTick(row)).toBeVisible()
      const calls = api.trackCalls()
      await locateEditingCross(row).click()
      await expect(row).toHaveText(new RegExp('^' + oldName))
      expect(calls.updateDirectory).toMatchObject([])
    }))

test('change to blank name (keyboard)', ({ page }) =>
  mockAllAndLogin({ page })
    .createFolder()
    .driveTable.withRows(async (rows) => {
      await locateAssetRowName(rows.nth(0)).click()
    })
    .press('Mod+R')
    .driveTable.withRows(async (rows, _, { api }) => {
      const row = rows.nth(0)
      const nameEl = locateAssetRowName(row)
      const oldName = (await nameEl.textContent()) ?? ''
      await nameEl.getByTestId('input').fill('')
      const calls = api.trackCalls()
      await nameEl.press('Enter')
      await expect(row).toHaveText(new RegExp('^' + oldName))
      expect(calls.updateDirectory).toMatchObject([])
    }))

test('edit name, error message is visible', ({ page }) => {
  return mockAllAndLogin({
    page,
    setupAPI: (api) => {
      for (let i = 0; i < 100; i++) {
        api.addProject({ title: 'Some Project ' + i })
      }
    },
  }).driveTable.withRows(async (rows) => {
    const row = rows.nth(0)
    await locateAssetRowName(row).click()

    const nameEl = locateAssetRowName(row)
    await nameEl.click()
    await nameEl.click()

    const inputEl = locateInput(nameEl)

    await inputEl.fill('')

    await locateEditingTick(row).click()

    const formElement = row.getByTestId('editable-span-form')
    const errorOutline = formElement.getByTestId('error-message-outline')
    const errorText = formElement.getByTestId('error-message-text')

    await expect(errorOutline).toBeVisible()
    // Clicking the element to be sure it's not overlapped by another element.
    await errorText.click()

    await inputEl.fill('Another Project')
    await locateEditingTick(row).click()

    await expect(errorOutline).not.toBeAttached()
    await expect(errorText).not.toBeAttached()
  })
})

test('edit name (empty name)', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addProject({ title: 'Some Project' })
      api.addProject({ title: 'Other Project' })
      api.addProject({ title: 'Yet Another Project' })
    },
  }).driveTable.withRows(async (rows) => {
    const row = rows.nth(0)
    await locateAssetRowName(row).click()

    const nameEl = locateAssetRowName(row)
    await nameEl.click()
    await nameEl.click()

    const inputEl = locateInput(nameEl)

    await inputEl.fill('')

    await locateEditingTick(row).click()

    const formElement = row.getByTestId('editable-span-form')
    const errorOutline = formElement.getByTestId('error-message-outline')
    const errorContainer = formElement.getByTestId('error-message-container')
    const errorText = formElement.getByTestId('error-message-text')

    await expect(errorOutline).toBeVisible()
    await expect(errorContainer).toBeVisible()
    await expect(errorText).toHaveText(getText('arbitraryFieldRequired'))

    await inputEl.fill('Another Project')
    await locateEditingTick(row).click()

    await expect(row).toHaveText(/^Another Project/)
    await expect(errorOutline).not.toBeAttached()
    await expect(errorContainer).not.toBeAttached()
  }))

test('edit name (invalid name)', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addDirectory({ title: 'Some Directory' })
    },
  }).driveTable.withRows(async (rows) => {
    const row = rows.nth(0)
    await locateAssetRowName(row).click()

    const nameEl = locateAssetRowName(row)
    await nameEl.click()
    await nameEl.click()

    const inputEl = locateInput(nameEl)

    await inputEl.fill('../')

    await locateEditingTick(row).click()

    const formElement = row.getByTestId('editable-span-form')
    const errorOutline = formElement.getByTestId('error-message-outline')
    const errorContainer = formElement.getByTestId('error-message-container')
    const errorText = formElement.getByTestId('error-message-text')

    await expect(errorOutline).toBeVisible()
    await expect(errorContainer).toBeVisible({
      visible: true,
    })
    await expect(errorText).toHaveText(getText('nameShouldNotContainInvalidCharacters'))

    await inputEl.fill('Other Directory')
    await locateEditingTick(row).click()

    await expect(row).toHaveText(/^Other Directory/)
  }))

test('edit name (duplicate name)', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addDirectory({ title: 'Some Directory' })
      api.addProject({ title: 'Some Project' })
    },
  }).driveTable.withRows(async (rows) => {
    const row = rows.nth(0)
    await locateAssetRowName(row).click()

    const nameEl = locateAssetRowName(row)
    await nameEl.click()
    await nameEl.click()

    const inputEl = locateInput(nameEl)

    await inputEl.fill('Some Project')
    await locateEditingTick(row).click()

    const formElement = row.getByTestId('editable-span-form')
    const errorText = formElement.getByTestId('error-message-text')

    await expect(errorText).toHaveText(getText('nameShouldBeUnique'))

    await inputEl.fill('Other Directory')
    await locateEditingTick(row).click()

    await expect(row).toHaveText(/^Other Directory/)
  }))

test('error should not overlay the table header', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      for (let i = 0; i < 100; i++) {
        api.addProject({ title: 'Some Project ' + i })
      }
    },
  }).driveTable.withRows(async (rows) => {
    const row = rows.nth(1)
    await locateAssetRowName(row).click()

    const nameEl = locateAssetRowName(row)
    await nameEl.click()
    await nameEl.click()

    const inputEl = locateInput(nameEl)

    await inputEl.fill('Some Project 0')
    await locateEditingTick(row).click()

    const formElement = row.getByTestId('editable-span-form')
    const errorOutline = formElement.getByTestId('error-message-outline')
    const errorContainer = formElement.getByTestId('error-message-container')
    const errorText = formElement.getByTestId('error-message-text')

    await expect(errorText).toHaveText(getText('nameShouldBeUnique'))

    await rows.nth(51).scrollIntoViewIfNeeded()

    await expect(errorOutline).not.toBeInViewport()
    await expect(errorContainer).not.toBeInViewport()
    await expect(errorText).not.toBeInViewport()
  }))
