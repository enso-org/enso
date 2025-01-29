/** @file Test copying, moving, cutting and pasting. */
import { expect, test } from '@playwright/test'

import { mockAllAndLogin } from './actions'

/** The name of the uploaded file. */
const FILE_NAME = 'foo.txt'
/** The contents of the uploaded file. */
const FILE_CONTENTS = 'hello world'
/** The name of the created secret. */
const SECRET_NAME = 'a secret name'
/** The value of the created secret. */
const SECRET_VALUE = 'a secret value'

test('create folder', ({ page }) =>
  mockAllAndLogin({ page })
    .createFolder()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
      await expect(rows.nth(0)).toBeVisible()
      await expect(rows.nth(0)).toHaveText(/^New Folder 1/)
    }))

test('create project', ({ page }) =>
  mockAllAndLogin({ page })
    .newEmptyProject()
    // FIXME[sb]: https://github.com/enso-org/cloud-v2/issues/1615
    // Uncomment once cloud execution in the browser is re-enabled.
    // .do((thePage) => expect(locateEditor(thePage)).toBeAttached())
    // .goToPage.drive()
    .driveTable.withRows((rows) => expect(rows).toHaveCount(1)))

test('upload file', ({ page }) =>
  mockAllAndLogin({ page })
    .uploadFile(FILE_NAME, FILE_CONTENTS)
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
      await expect(rows.nth(0)).toBeVisible()
      await expect(rows.nth(0)).toHaveText(new RegExp('^' + FILE_NAME))
    }))

test('create secret', ({ page }) =>
  mockAllAndLogin({ page })
    .createSecret(SECRET_NAME, SECRET_VALUE)
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
      await expect(rows.nth(0)).toBeVisible()
      await expect(rows.nth(0)).toHaveText(new RegExp('^' + SECRET_NAME))
    }))
