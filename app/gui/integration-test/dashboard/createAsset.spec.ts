/** @file Test copying, moving, cutting and pasting. */
import { expect, test } from 'integration-test/base'

/** The name of the uploaded file. */
const FILE_NAME = 'foo.txt'
/** The contents of the uploaded file. */
const FILE_CONTENTS = 'hello world'
/** The name of the created secret. */
const SECRET_NAME = 'a secret name'
/** The value of the created secret. */
const SECRET_VALUE = 'a secret value'

test('create folder (remote)', async ({ drivePage }) => {
  await drivePage.goToCategory
    .cloud()
    .createFolder()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
      await expect(rows.nth(0)).toBeVisible()
      await expect(rows.nth(0)).toHaveText(/^New Folder 1/)
    })
})

test('create folder (local)', async ({ drivePage }) => {
  await drivePage.goToCategory
    .cloud()
    .goToCategory.local()
    .createFolder()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(2)
      await expect(rows.nth(0)).toBeVisible()
      await expect(rows.nth(0)).toHaveText(/^New Folder 1/)
    })
})

test('create project (remote)', async ({ drivePage }) => {
  await drivePage.goToCategory
    .cloud()
    .newEmptyProject()
    .waitForEditorToLoad()
    .goToPage.drive()
    // Entries: [Samples, New Project 2]
    .driveTable.withRows((rows) => expect(rows).toHaveCount(1))
})

test('create project (local)', async ({ drivePage }) => {
  await drivePage.goToCategory
    .local()
    .newEmptyProject()
    .waitForEditorToLoad()
    .goToPage.drive()
    .driveTable.withRows((rows) => expect(rows).toHaveCount(2))
})

test('upload file (remote)', async ({ drivePage }) => {
  await drivePage.goToCategory
    .cloud()
    .uploadFile(FILE_NAME, FILE_CONTENTS)
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
      await expect(rows.nth(0)).toBeVisible()
      await expect(rows.nth(0)).toHaveText(new RegExp('^' + FILE_NAME))
    })
})

test('upload file (local)', async ({ drivePage }) => {
  await drivePage.goToCategory
    .local()
    .uploadFile(FILE_NAME, FILE_CONTENTS)
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(2)
      await expect(rows.nth(1)).toBeVisible()
      await expect(rows.nth(1)).toHaveText(new RegExp('^' + FILE_NAME))
    })
})

test('create secret (remote)', async ({ drivePage }) => {
  await drivePage.goToCategory
    .cloud()
    .createSecret(SECRET_NAME, SECRET_VALUE)
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
      await expect(rows.nth(0)).toBeVisible()
      await expect(rows.nth(0)).toHaveText(new RegExp('^' + SECRET_NAME))
    })
})
