/** @file Test the drive view. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test('extra columns should stick to right side of assets table', async ({ page }) => {
  await actions.mockAllAndLogin({ page })
  await actions.locateAccessedByProjectsColumnToggle(page).click()
  await actions.locateAccessedDataColumnToggle(page).click()
  await actions.locateAssetsTable(page).evaluate(element => {
    let scrollableParent: HTMLElement | SVGElement | null = element
    while (
      scrollableParent != null &&
      scrollableParent.scrollWidth <= scrollableParent.clientWidth
    ) {
      scrollableParent = scrollableParent.parentElement
    }
    // eslint-disable-next-line @typescript-eslint/no-magic-numbers
    scrollableParent?.scrollTo({ left: 999999, behavior: 'instant' })
  })
  const extraColumns = actions.locateExtraColumns(page)
  const assetsTable = actions.locateAssetsTable(page)
  await test
    .expect(async () => {
      const extraColumnsRight = await extraColumns.evaluate(
        element => element.getBoundingClientRect().right
      )
      const assetsTableRight = await assetsTable.evaluate(
        element => element.getBoundingClientRect().right
      )
      test.expect(extraColumnsRight).toEqual(assetsTableRight)
    })
    .toPass()
})

test.test('extra columns should stick to top of scroll container', async ({ page }) => {
  const { api } = await actions.mockAllAndLogin({ page })
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  for (let i = 0; i < 100; i += 1) {
    api.addFile('a')
  }
  await actions.login({ page })

  await actions.locateAccessedByProjectsColumnToggle(page).click()
  await actions.locateAccessedDataColumnToggle(page).click()
  await actions.locateAssetsTable(page).evaluate(element => {
    let scrollableParent: HTMLElement | SVGElement | null = element
    while (
      scrollableParent != null &&
      scrollableParent.scrollWidth <= scrollableParent.clientWidth
    ) {
      scrollableParent = scrollableParent.parentElement
    }
    // eslint-disable-next-line @typescript-eslint/no-magic-numbers
    scrollableParent?.scrollTo({ top: 999999, behavior: 'instant' })
  })
  const extraColumns = actions.locateExtraColumns(page)
  const assetsTable = actions.locateAssetsTable(page)
  await test
    .expect(async () => {
      const extraColumnsTop = await extraColumns.evaluate(
        element => element.getBoundingClientRect().top
      )
      const assetsTableTop = await assetsTable.evaluate(element => {
        let scrollableParent: HTMLElement | SVGElement | null = element
        while (
          scrollableParent != null &&
          scrollableParent.scrollWidth <= scrollableParent.clientWidth
        ) {
          scrollableParent = scrollableParent.parentElement
        }
        return scrollableParent?.getBoundingClientRect().top
      })
      test.expect(extraColumnsTop).toEqual(assetsTableTop)
    })
    .toPass()
})

test.test('can drop onto root directory dropzone', async ({ page }) => {
  const { api } = await actions.mockAllAndLogin({ page })
  const assetRows = actions.locateAssetRows(page)
  const asset = api.addDirectory('a')
  api.addFile('b', { parentId: asset.id })
  await actions.login({ page })

  await assetRows.nth(0).dblclick()
  const parentLeft = await actions.getAssetRowLeftPx(assetRows.nth(0))
  const childLeft = await actions.getAssetRowLeftPx(assetRows.nth(1))
  test.expect(childLeft, 'child is indented further than parent').toBeGreaterThan(parentLeft)
  await assetRows.nth(1).dragTo(actions.locateRootDirectoryDropzone(page))
  const firstLeft = await actions.getAssetRowLeftPx(assetRows.nth(0))
  const secondLeft = await actions.getAssetRowLeftPx(assetRows.nth(1))
  test.expect(firstLeft, 'siblings have same indentation').toEqual(secondLeft)
})
