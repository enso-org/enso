/** @file Test dragging of labels. */
import * as test from '@playwright/test'

import * as backend from '#/services/backend'

import * as actions from './actions'

test.test('drag labels', async ({ page }) => {
    const { api } = await actions.mockAllAndLogin({ page })
    const assetRows = actions.locateAssetRows(page)
    const labels = actions.locateLabelsPanelLabels(page)
    const firstLabel = 'aaaa'
    api.addLabel(firstLabel, backend.COLORS[0])
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

    await page.keyboard.down(await actions.modModifier(page))
    await assetRows.nth(0).click()
    await assetRows.nth(2).click()
    await labels.nth(0).dragTo(assetRows.nth(2))
    await test
        .expect(actions.locateAssetLabels(assetRows.nth(0)).getByText(firstLabel))
        .toBeVisible()
    await test
        .expect(actions.locateAssetLabels(assetRows.nth(1)).getByText(firstLabel))
        .not.toBeVisible()
    await test
        .expect(actions.locateAssetLabels(assetRows.nth(2)).getByText(firstLabel))
        .toBeVisible()
    await test
        .expect(actions.locateAssetLabels(assetRows.nth(3)).getByText(firstLabel))
        .not.toBeVisible()
})
