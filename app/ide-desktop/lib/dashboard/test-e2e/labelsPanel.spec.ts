/** @file Test the labels sidebar panel. */
import * as test from '@playwright/test'

import * as actions from './actions'
import * as api from './api'

test.test('labels', async ({ page }) => {
  await api.mockApi(page)
  await actions.login(page)

  // Screenshot #1: Empty labels panel
  await test.expect(actions.locateLabelsPanel(page)).toHaveScreenshot()

  // Screenshot #2: "Create label" modal
  await actions.locateNewLabelButton(page).click()
  await test.expect(actions.locateNewLabelModal(page)).toHaveScreenshot()

  // Screenshot #3: "Create label" modal with name set
  await actions.locateNewLabelModalNameInput(page).fill('New Label')
  await test.expect(actions.locateNewLabelModal(page)).toHaveScreenshot()

  await page.press('body', 'Escape')

  // Screenshot #4: "Create label" modal with color set
  // The exact number is allowed to vary; but to click the fourth color, there must be at least
  // four colors.
  await actions.locateNewLabelButton(page).click()
  test.expect(await actions.locateNewLabelModalColorButtons(page).count()).toBeGreaterThanOrEqual(4)
  // `force: true` is required because the `label` needs to handle the click event, not the
  // `button`.
  await actions.locateNewLabelModalColorButtons(page).nth(4).click({ force: true })
  await test.expect(actions.locateNewLabelModal(page)).toHaveScreenshot()

  // Screenshot #5: "Create label" modal with name and color set
  await actions.locateNewLabelModalNameInput(page).fill('New Label')
  await test.expect(actions.locateNewLabelModal(page)).toHaveScreenshot()

  // Screenshot (flaky, omitted): Labels panel with one entry
  await actions.locateCreateButton(actions.locateNewLabelModal(page)).click()
  // await test.expect(actions.locateLabelsPanel(page)).toHaveScreenshot()

  // Screenshot #6: Empty labels panel again, after deleting the only entry
  // This uses a screenshot instead of `toHaveCount(count)` because it is less prone to breakage
  // and easier to maintain.
  await actions.locateLabelsPanelLabels(page).first().hover()
  await actions.locateDeleteIcon(actions.locateLabelsPanel(page)).first().click()
  await actions.locateDeleteButton(page).click()
  await test.expect(actions.locateLabelsPanel(page)).toHaveScreenshot()
})
