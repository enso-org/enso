/** @file Test dragging of labels. */
import * as test from '@playwright/test'

import * as backend from '#/services/Backend'

import * as actions from './actions'

test.test('drag labels onto single row', async ({ page }) => {
  const { api } = await actions.mockAllAndLogin({ page })
  const assetRows = actions.locateAssetRows(page)
  const labels = actions.locateLabelsPanelLabels(page)
  const label = 'aaaa'
  api.addLabel(label, backend.COLORS[0])
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  api.addLabel('bbbb', backend.COLORS[1]!)
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  api.addLabel('cccc', backend.COLORS[2]!)
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  api.addLabel('dddd', backend.COLORS[3]!)
  api.addDirectory('foo')
  api.addSecret('bar')
  api.addFile('baz')
  api.addSecret('quux')
  await actions.login({ page })

  await actions.locateLabelsColumnToggle(page).click()
  await labels.nth(0).dragTo(assetRows.nth(1))
  await test.expect(actions.locateAssetLabels(assetRows.nth(0)).getByText(label)).not.toBeVisible()
  await test.expect(actions.locateAssetLabels(assetRows.nth(1)).getByText(label)).toBeVisible()
  await test.expect(actions.locateAssetLabels(assetRows.nth(2)).getByText(label)).not.toBeVisible()
  await test.expect(actions.locateAssetLabels(assetRows.nth(3)).getByText(label)).not.toBeVisible()
})

test.test('drag labels onto multiple rows', async ({ page }) => {
  const { api } = await actions.mockAllAndLogin({ page })
  const assetRows = actions.locateAssetRows(page)
  const labels = actions.locateLabelsPanelLabels(page)
  const label = 'aaaa'
  api.addLabel(label, backend.COLORS[0])
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  api.addLabel('bbbb', backend.COLORS[1]!)
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  api.addLabel('cccc', backend.COLORS[2]!)
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  api.addLabel('dddd', backend.COLORS[3]!)
  api.addDirectory('foo')
  api.addSecret('bar')
  api.addFile('baz')
  api.addSecret('quux')
  await actions.login({ page })

  await actions.locateLabelsColumnToggle(page).click()
  await page.keyboard.down(await actions.modModifier(page))
  await assetRows.nth(0).click()
  await assetRows.nth(2).click()
  await labels.nth(0).dragTo(assetRows.nth(2))
  await page.keyboard.up(await actions.modModifier(page))
  await test.expect(actions.locateAssetLabels(assetRows.nth(0)).getByText(label)).toBeVisible()
  await test.expect(actions.locateAssetLabels(assetRows.nth(1)).getByText(label)).not.toBeVisible()
  await test.expect(actions.locateAssetLabels(assetRows.nth(2)).getByText(label)).toBeVisible()
  await test.expect(actions.locateAssetLabels(assetRows.nth(3)).getByText(label)).not.toBeVisible()
})

test.test('drag (recursive)', async ({ page }) => {
  const { api } = await actions.mockAllAndLogin({ page })
  const assetRows = actions.locateAssetRows(page)
  const labels = actions.locateLabelsPanelLabels(page)
  const label = 'bbbb'
  api.addLabel('aaaa', backend.COLORS[0])
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  api.addLabel(label, backend.COLORS[1]!)
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  api.addLabel('cccc', backend.COLORS[2]!)
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  api.addLabel('dddd', backend.COLORS[3]!)
  const assetsWithLabel = new Set<string>()
  const shouldHaveLabel = <T extends backend.AnyAsset>(asset: T) => {
    assetsWithLabel.add(asset.title)
    return asset
  }
  const directory1 = shouldHaveLabel(api.addDirectory('foo'))
  const directory2 = shouldHaveLabel(api.addDirectory('bar'))
  shouldHaveLabel(api.addFile('baz', { parentId: directory1.id }))
  shouldHaveLabel(api.addSecret('quux', { parentId: directory1.id }))
  const directory3 = api.addDirectory('directory 3')
  const directory5 = shouldHaveLabel(api.addDirectory('directory 5', { parentId: directory3.id }))
  api.addFile('file 1', { parentId: directory3.id })
  api.addProject('file 2', { parentId: directory3.id })
  api.addFile('another file')
  const directory4 = shouldHaveLabel(api.addDirectory('blargle', { parentId: directory2.id }))
  shouldHaveLabel(api.addProject('abcd', { parentId: directory2.id }))
  shouldHaveLabel(api.addProject('efgh', { parentId: directory2.id }))
  shouldHaveLabel(api.addFile('ijkl', { parentId: directory4.id }))
  shouldHaveLabel(api.addProject('mnop', { parentId: directory4.id }))
  shouldHaveLabel(api.addSecret('secret 1', { parentId: directory5.id }))
  shouldHaveLabel(api.addFile('yet another file', { parentId: directory5.id }))
  await actions.login({ page })

  await actions.locateLabelsColumnToggle(page).click()
  let didExpandRows = false
  do {
    didExpandRows = false
    const directories = await actions.locateExpandableDirectories(page).all()
    // If going through the directories in forward order, the positions change when
    // one directory is expanded, making the double click happend on the wrong row
    // for all directories after it.
    for (const directory of directories.reverse()) {
      didExpandRows = true
      await directory.dblclick()
    }
  } while (didExpandRows)
  await page.keyboard.down(await actions.modModifier(page))
  const directory1Row = assetRows.filter({ hasText: directory1.title })
  await directory1Row.click()
  await assetRows.filter({ hasText: directory2.title }).click()
  await assetRows.filter({ hasText: directory5.title }).click()
  await labels.nth(1).dragTo(directory1Row)
  await page.keyboard.up(await actions.modModifier(page))
  for (const row of await actions.locateAssetRows(page).all()) {
    const name = await actions.locateAssetName(row).innerText()
    const labelElement = actions.locateAssetLabels(row).getByText(label)
    if (assetsWithLabel.has(name)) {
      await test.expect(labelElement).toBeVisible()
    } else {
      await test.expect(labelElement).not.toBeVisible()
    }
  }
})

test.test('drag (inverted, recursive)', async ({ page }) => {
  const { api } = await actions.mockAllAndLogin({ page })
  const assetRows = actions.locateAssetRows(page)
  const labels = actions.locateLabelsPanelLabels(page)
  const label = 'bbbb'
  api.addLabel('aaaa', backend.COLORS[0])
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  const backendLabel = api.addLabel(label, backend.COLORS[1]!)
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  api.addLabel('cccc', backend.COLORS[2]!)
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  api.addLabel('dddd', backend.COLORS[3]!)
  const assetsWithoutLabel = new Set<string>()
  const shouldNotHaveLabel = <T extends backend.AnyAsset>(asset: T) => {
    assetsWithoutLabel.add(asset.title)
    return asset
  }
  const directory1 = shouldNotHaveLabel(api.addDirectory('foo'))
  const directory2 = shouldNotHaveLabel(api.addDirectory('bar'))
  shouldNotHaveLabel(api.addFile('baz', { parentId: directory1.id }))
  shouldNotHaveLabel(api.addSecret('quux', { parentId: directory1.id }))
  const directory3 = api.addDirectory('directory 3')
  const directory5 = shouldNotHaveLabel(
    api.addDirectory('directory 5', { parentId: directory3.id })
  )
  api.addFile('file 1', { parentId: directory3.id })
  api.addProject('file 2', { parentId: directory3.id })
  api.addFile('another file')
  const directory4 = shouldNotHaveLabel(api.addDirectory('blargle', { parentId: directory2.id }))
  shouldNotHaveLabel(api.addProject('abcd', { parentId: directory2.id }))
  shouldNotHaveLabel(api.addProject('efgh', { parentId: directory2.id }))
  shouldNotHaveLabel(api.addFile('ijkl', { parentId: directory4.id }))
  shouldNotHaveLabel(api.addProject('mnop', { parentId: directory4.id }))
  shouldNotHaveLabel(api.addSecret('secret 1', { parentId: directory5.id }))
  shouldNotHaveLabel(api.addFile('yet another file', { parentId: directory5.id }))
  api.setLabels(api.rootDirectoryId, [backendLabel.value])
  await actions.login({ page })

  await actions.locateLabelsColumnToggle(page).click()
  /** The default position (the center) cannot be clicked on as it lands exactly on a label -
   * which has its own mouse action. It also cannot be too far left, otherwise it triggers
   * edit mode for the name. */
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  const somewhereInRow = { x: 300, y: 16 }
  let didExpandRows = false
  do {
    didExpandRows = false
    const directories = await actions.locateExpandableDirectories(page).all()
    // If going through the directories in forward order, the positions change when
    // one directory is expanded, making the double click happend on the wrong row
    // for all directories after it.
    for (const directory of directories.reverse()) {
      didExpandRows = true
      await directory.dblclick({ position: somewhereInRow })
    }
  } while (didExpandRows)
  await page.keyboard.down(await actions.modModifier(page))
  const directory1Row = assetRows.filter({ hasText: directory1.title })
  await directory1Row.click({ position: somewhereInRow })
  await assetRows.filter({ hasText: directory2.title }).click({ position: somewhereInRow })
  await assetRows.filter({ hasText: directory5.title }).click({ position: somewhereInRow })
  await labels.nth(1).dragTo(directory1Row)
  await page.keyboard.up(await actions.modModifier(page))
  for (const row of await actions.locateAssetRows(page).all()) {
    const name = await actions.locateAssetName(row).innerText()
    const labelElement = actions.locateAssetLabels(row).getByText(label)
    if (assetsWithoutLabel.has(name)) {
      await test.expect(labelElement).not.toBeVisible()
    } else {
      await test.expect(labelElement).toBeVisible()
    }
  }
})
