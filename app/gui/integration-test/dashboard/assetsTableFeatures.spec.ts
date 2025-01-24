/** @file Test the drive view. */
import { expect, test, type Locator, type Page } from '@playwright/test'

import { EmailAddress, ProjectState } from '#/services/Backend'
import { getText, mockAllAndLogin, TEXT } from './actions'

/** Find an extra columns button panel. */
function locateExtraColumns(page: Page) {
  // This has no identifying features.
  return page.getByTestId('extra-columns')
}

/**
 * Get the left side of the bounding box of an asset row. The locator MUST be for an asset row.
 * DO NOT assume the left side of the outer container will change. This means that it is NOT SAFE
 * to do anything with the returned values other than comparing them.
 */
function getAssetRowLeftPx(locator: Locator) {
  return locator.evaluate((el) => el.children[0]?.children[0]?.getBoundingClientRect().left ?? 0)
}

/**
 * Find a root directory dropzone.
 * This is the empty space below the assets table, if it doesn't take up the whole screen
 * vertically.
 */
function locateRootDirectoryDropzone(page: Page) {
  // This has no identifying features.
  return page.getByTestId('root-directory-dropzone')
}

const PASS_TIMEOUT = 5_000

test('extra columns should stick to top of scroll container', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      for (let i = 0; i < 100; i += 1) {
        api.addFile({ title: 'a' })
      }
    },
  })
    .withAssetsTable(async (assetsTable) => {
      await assetsTable.evaluate((element) => {
        let scrollableParent: HTMLElement | SVGElement | null = element
        while (
          scrollableParent != null &&
          scrollableParent.scrollHeight <= scrollableParent.clientHeight
        ) {
          scrollableParent = scrollableParent.parentElement
        }
        scrollableParent?.scrollTo({ top: 999999, behavior: 'instant' })
      })
    })
    .withAssetsTable(async (assetsTable, _, thePage) => {
      const extraColumns = locateExtraColumns(thePage)
      await expect(async () => {
        const extraColumnsTop = await extraColumns.evaluate(
          (element) => element.getBoundingClientRect().top,
        )
        const assetsTableTop = await assetsTable.evaluate((element) => {
          let scrollableParent: HTMLElement | SVGElement | null = element
          while (
            scrollableParent != null &&
            scrollableParent.scrollHeight <= scrollableParent.clientHeight
          ) {
            scrollableParent = scrollableParent.parentElement
          }
          return scrollableParent?.getBoundingClientRect().top ?? 0
        })
        expect(extraColumnsTop).toEqual(assetsTableTop + 2)
      }).toPass({ timeout: PASS_TIMEOUT })
    }))

test('can drop onto root directory dropzone', ({ page }) =>
  mockAllAndLogin({ page })
    .createFolder()
    .uploadFile('b', 'testing')
    .driveTable.doubleClickRow(0)
    .driveTable.withRows(async (rows, nonAssetRows) => {
      const parentLeft = await getAssetRowLeftPx(rows.nth(0))
      await expect(nonAssetRows.nth(0)).toHaveText(TEXT.thisFolderIsEmpty)
      const childLeft = await getAssetRowLeftPx(nonAssetRows.nth(0))
      expect(childLeft, 'Child is indented further than parent').toBeGreaterThan(parentLeft)
    })
    .driveTable.dragRow(1, locateRootDirectoryDropzone(page))
    .driveTable.withRows(async (rows) => {
      const firstLeft = await getAssetRowLeftPx(rows.nth(0))
      const secondLeft = await getAssetRowLeftPx(rows.nth(1))
      expect(firstLeft, 'Siblings have same indentation').toEqual(secondLeft)
    }))

test('can navigate to parent directory of an asset in the Recent category', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addProject({ title: 'a' })
      api.addProject({ title: 'b' })

      const directory = api.addDirectory({ title: 'd' })
      const subDirectory = api.addDirectory({ title: 'e', parentId: directory.id })

      api.addProject({ title: 'c', parentId: subDirectory.id })
    },
  })
    .driveTable.expandDirectory(0)
    .driveTable.expandDirectory(1)
    // Project in the nested directory (c)
    .driveTable.rightClickRow(2)
    .contextMenu.moveNonFolderToTrash()
    // Project in the root (a)
    .driveTable.rightClickRow(2)
    .contextMenu.moveNonFolderToTrash()
    .goToCategory.trash()
    .driveTable.withPathColumnCell('a', async (cell) => {
      await expect(cell).toBeVisible()

      await cell.getByRole('button').click()

      await expect(cell).not.toBeVisible()
    })
    .expectCategory(TEXT.cloudCategory)
    .goToCategory.trash()
    .driveTable.withPathColumnCell('c', async (cell) => {
      await expect(cell).toBeVisible()

      await cell.getByRole('button').click()

      await page.getByTestId('path-column-item-d').click()
    })
    .expectCategory(TEXT.cloudCategory)
    .driveTable.withSelectedRows(async (rows) => {
      await expect(rows).toHaveCount(1)
      await expect(rows.nth(0)).toHaveText(/^d/)
    }))

test("can't run a project in browser by default", ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: async (api) => {
      api.addProject({ title: 'a' })
    },
  }).driveTable.withRows(async (rows) => {
    const row = rows.first()

    const startProjectButton = row.getByTestId('open-project')
    await expect(startProjectButton).toBeDisabled()
  }))

test("can't start an already running by another user", ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: async (api) => {
      await api.setFeatureFlags({ enableCloudExecution: true })

      const userGroup = api.addUserGroup('Test Group')

      api.addUserGroupToUser(api.defaultUser.userId, userGroup.id)

      const peer = api.addUser('Test User', {
        email: EmailAddress('test@test.com'),
        userGroups: [userGroup.id],
      })

      api.addProject({
        title: 'a',
        projectState: {
          type: ProjectState.opened,
          volumeId: '123',
          openedBy: peer.email,
        },
      })
    },
  }).driveTable.withRows(async (rows) => {
    const row = rows.first()
    const startProjectButton = row.getByTestId('open-project')
    const stopProjectButton = row.getByTestId('stop-project')

    await expect(row).toBeVisible()
    await expect(row.getByTestId('switch-to-project')).not.toBeVisible()
    await expect(startProjectButton).not.toBeVisible()
    await expect(stopProjectButton).toBeDisabled()
    await expect(stopProjectButton).toHaveAccessibleName(getText('xIsUsingTheProject', 'Test User'))
  }))
