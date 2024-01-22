/** @file Test copying, moving, cutting and pasting. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test.beforeEach(actions.mockAllAndLogin)

test.test('create folder', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)

  await actions.locateNewFolderIcon(page).click()
  // Assets: [0: Folder 1]
  await test.expect(assetRows).toHaveCount(1)
  await test.expect(assetRows.nth(0)).toBeVisible()
  await test.expect(assetRows.nth(0)).toHaveText(/^New Folder 1/)
})

test.test('create project', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)

  await actions.locateNewProjectButton(page).click()
  // Assets: [0: Project 1]
  await test.expect(assetRows).toHaveCount(1)
  await test.expect(actions.locateEditor(page)).toBeVisible()
})

test.test('upload file', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)

  const fileChooserPromise = page.waitForEvent('filechooser')
  await actions.locateUploadFilesIcon(page).click()
  const fileChooser = await fileChooserPromise
  const name = 'foo.txt'
  const content = 'hello world'
  await fileChooser.setFiles([
    {
      name,
      buffer: Buffer.from(content),
      mimeType: 'text/plain',
    },
  ])

  await test.expect(assetRows).toHaveCount(1)
  await test.expect(assetRows.nth(0)).toBeVisible()
  await test.expect(assetRows.nth(0)).toHaveText(new RegExp('^' + name))
})

test.test('create secret', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)

  await actions.locateNewSecretIcon(page).click()
  const name = 'a secret name'
  const value = 'a secret value'
  await actions.locateSecretNameInput(page).fill(name)
  await actions.locateSecretValueInput(page).fill(value)
  await actions.locateCreateButton(page).click()
  await test.expect(assetRows).toHaveCount(1)
  await test.expect(assetRows.nth(0)).toBeVisible()
  await test.expect(assetRows.nth(0)).toHaveText(new RegExp('^' + name))
})
