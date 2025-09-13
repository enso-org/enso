/** @file Test copying, moving, cutting and pasting. */
import { expect, test } from 'playwright/test'

import { mockAllAndLogin } from './actions'

/** The name of the uploaded file. */
const FILE_NAME = 'foo.txt'
/** The contents of the uploaded file. */
const FILE_CONTENTS = 'hello world'
/** The name of the created secret. */
const SECRET_NAME = 'a secret name'
/** The value of the created secret. */
const SECRET_VALUE = 'a secret value'

test('create folder (remote)', ({ page }) =>
  mockAllAndLogin({ page })
    .createFolder()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
      await expect(rows.nth(0)).toBeVisible()
      await expect(rows.nth(0)).toHaveText(/^New Folder 1/)
    }))

test('create folder (local)', ({ page }) =>
  mockAllAndLogin({ page })
    .goToCategory.local()
    .createFolder()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
      await expect(rows.nth(0)).toBeVisible()
      await expect(rows.nth(0)).toHaveText(/^New Folder 1/)
    }))

test('create project (remote)', ({ page }) =>
  mockAllAndLogin({ page })
    .newEmptyProject()
    .waitForEditorToLoad()
    .goToPage.drive()
    // Entries: [Samples, New Project 2]
    .driveTable.withRows((rows) => expect(rows).toHaveCount(1)))

test('create project (local)', ({ page }) =>
  mockAllAndLogin({ page })
    .goToCategory.local()
    .newEmptyProject()
    .waitForEditorToLoad()
    .goToPage.drive()
    .driveTable.withRows((rows) => expect(rows).toHaveCount(1)))

test('upload file (remote)', ({ page }) =>
  mockAllAndLogin({ page })
    .uploadFile(FILE_NAME, FILE_CONTENTS)
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
      await expect(rows.nth(0)).toBeVisible()
      await expect(rows.nth(0)).toHaveText(new RegExp('^' + FILE_NAME))
    }))

test('upload file (local)', ({ page }) =>
  mockAllAndLogin({ page })
    .goToCategory.local()
    .uploadFile(FILE_NAME, FILE_CONTENTS)
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
      await expect(rows.nth(0)).toBeVisible()
      await expect(rows.nth(0)).toHaveText(new RegExp('^' + FILE_NAME))
    }))

test('create secret (remote)', ({ page }) =>
  mockAllAndLogin({ page })
    .createSecret(SECRET_NAME, SECRET_VALUE)
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
      await expect(rows.nth(0)).toBeVisible()
      await expect(rows.nth(0)).toHaveText(new RegExp('^' + SECRET_NAME))
    }))
