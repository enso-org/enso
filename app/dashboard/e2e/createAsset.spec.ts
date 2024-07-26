/** @file Test copying, moving, cutting and pasting. */
import * as test from '@playwright/test'

import * as actions from './actions'

// =================
// === Constants ===
// =================

/** The name of the uploaded file. */
const FILE_NAME = 'foo.txt'
/** The contents of the uploaded file. */
const FILE_CONTENTS = 'hello world'
/** The name of the created secret. */
const SECRET_NAME = 'a secret name'
/** The value of the created secret. */
const SECRET_VALUE = 'a secret value'

// =============
// === Tests ===
// =============

test.test('create folder', ({ page }) =>
  actions
    .mockAllAndLogin({ page })
    .createFolder()
    .driveTable.withRows(async (rows) => {
      await test.expect(rows).toHaveCount(1)
      await test.expect(rows.nth(0)).toBeVisible()
      await test.expect(rows.nth(0)).toHaveText(/^New Folder 1/)
    }),
)

test.test('create project', ({ page }) =>
  actions
    .mockAllAndLogin({ page })
    .newEmptyProject()
    .do((thePage) => test.expect(actions.locateEditor(thePage)).toBeAttached())
    .goToPage.drive()
    .driveTable.withRows((rows) => test.expect(rows).toHaveCount(1)),
)

test.test('upload file', ({ page }) =>
  actions
    .mockAllAndLogin({ page })
    .uploadFile(FILE_NAME, FILE_CONTENTS)
    .driveTable.withRows(async (rows) => {
      await test.expect(rows).toHaveCount(1)
      await test.expect(rows.nth(0)).toBeVisible()
      await test.expect(rows.nth(0)).toHaveText(new RegExp('^' + FILE_NAME))
    }),
)

test.test('create secret', ({ page }) =>
  actions
    .mockAllAndLogin({ page })
    .createSecret(SECRET_NAME, SECRET_VALUE)
    .driveTable.withRows(async (rows) => {
      await test.expect(rows).toHaveCount(1)
      await test.expect(rows.nth(0)).toBeVisible()
      await test.expect(rows.nth(0)).toHaveText(new RegExp('^' + SECRET_NAME))
    }),
)
