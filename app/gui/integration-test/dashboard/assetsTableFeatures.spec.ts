/** @file Test the drive view. */
import { EmailAddress, ProjectState } from 'enso-common/src/services/Backend'
import { expect, test, type Page } from 'integration-test/base'
import { getText, TEXT } from '../actions'

/** Find an extra columns button panel. */
function locateExtraColumns(page: Page) {
  // This has no identifying features.
  return page.getByTestId('extra-columns')
}

const PASS_TIMEOUT = 5_000
test.describe(() => {
  test.use({
    setupApi: {
      cloud: (cloudApi) => {
        for (let i = 0; i < 100; i += 1) {
          cloudApi.addFile({ title: 'a' })
        }
      },
    },
  })
  test('extra columns should stick to top of scroll container', async ({ drivePage }) => {
    await drivePage.goToCategory
      .cloud()
      .driveTable.toggleColumn.labels()
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
      .withAssetsTable(async (assetsTable, _, page) => {
        const extraColumns = locateExtraColumns(page)
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
      })
  })
})

test.describe(() => {
  test.use({
    setupApi: {
      cloud: (cloudApi) => {
        cloudApi.addProject({ title: 'a' })
        cloudApi.addProject({ title: 'b' })
        const directory = cloudApi.addDirectory({ title: 'd' })
        const subDirectory = cloudApi.addDirectory({ title: 'e', parentId: directory.id })
        cloudApi.addProject({ title: 'c', parentId: subDirectory.id })
      },
    },
  })

  test('can navigate to parent directory of an asset in the Trash category', async ({
    drivePage,
    page,
  }) => {
    await drivePage.goToCategory
      .cloud()
      // Project in the root (a)
      .driveTable.rightClickRow('a')
      .contextMenu.moveToTrash()
      .driveTable.openDirectory('d')
      .driveTable.openDirectory('e')
      // Project in the nested directory (c)
      .driveTable.rightClickRow('c')
      .contextMenu.moveToTrash()
      .goToCategory.trash()
      .driveTable.withPathColumnCell('a', async (cell) => {
        await expect(cell).toBeVisible()

        await cell.getByRole('button').click()

        await expect(cell).toBeHidden()
      })
      .expectCategory(TEXT.cloudCategory)
      .goToCategory.trash()
      .driveTable.withPathColumnCell('c', async (cell) => {
        await expect(cell).toBeVisible()

        await cell.getByRole('button').click()

        await page.getByTestId('path-column-item-d').click()
      })
      .expectCategory(TEXT.cloudCategory)
  })
})
test.describe(() => {
  test.use({
    featureFlags: {
      enableLocalBackend: false,
      enableCloudExecution: false,
    },
  })
  test("can't run a project in browser by default", async ({ drivePage }) => {
    await drivePage.do((page) =>
      expect(page.getByText(TEXT.cloudBrowserDisabledTitle)).toBeVisible(),
    )
  })
})
test.describe(() => {
  test.use({
    setupApi: {
      cloud: (cloudApi) => {
        const userGroup = cloudApi.addUserGroup('Test Group')
        cloudApi.addUserGroupToUser(cloudApi.defaultUser.userId, userGroup.id)
        const peer = cloudApi.addUser('Test User', {
          email: EmailAddress('test@test.com'),
          userGroups: [userGroup.id],
        })
        cloudApi.addProject({
          title: 'a',
          projectState: {
            type: ProjectState.opened,
            volumeId: '123',
            openedBy: peer.email,
          },
        })
      },
    },
  })

  test("can't start a project that's opened by another user", async ({ drivePage }) => {
    await drivePage.goToCategory.cloud().driveTable.withRows(async (rows) => {
      const row = rows.first()
      const startProjectButton = row.getByTestId('open-project')
      const stopProjectButton = row.getByTestId('stop-project')

      await expect(row).toBeVisible()
      await expect(row.getByTestId('switch-to-project')).toBeHidden()
      await expect(startProjectButton).toBeHidden()
      await expect(stopProjectButton).toBeDisabled()
      await expect(stopProjectButton).toHaveAccessibleName(
        getText('xIsUsingTheProject', 'test@test.com'),
      )
    })
  })
})
