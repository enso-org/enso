/** @file Test the drive view. */
import { expect, test, type Page } from '@playwright/test'

import { EmailAddress, ProjectState } from '#/services/Backend'
import { getText, mockAllAndLogin, TEXT } from './actions'

/** Find an extra columns button panel. */
function locateExtraColumns(page: Page) {
  // This has no identifying features.
  return page.getByTestId('extra-columns')
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

test('can navigate to parent directory of an asset in the Trash category', ({ page }) =>
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
    // Project in the root (a)
    .driveTable.rightClickRow('a')
    .contextMenu.moveNonFolderToTrash()
    .driveTable.openDirectory('d')
    .driveTable.openDirectory('e')
    // Project in the nested directory (c)
    .driveTable.rightClickRow('c')
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
    .expectCategory(TEXT.cloudCategory))

test("can't run a project in browser by default", ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: async (api) => {
      api.addProject({ title: 'a' })
      api.setFeatureFlags({ enableCloudExecution: false })
    },
  }).do(() => {
    expect(page.getByText(TEXT.cloudBrowserDisabledTitle)).toBeVisible()
  }))

test("can't start an already running by another user", ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: async (api) => {
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
    await expect(stopProjectButton).toHaveAccessibleName(
      getText('xIsUsingTheProject', 'test@test.com'),
    )
  }))
