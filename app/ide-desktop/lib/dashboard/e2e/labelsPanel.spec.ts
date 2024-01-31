/** @file Test the labels sidebar panel. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test.beforeEach(actions.mockAllAndLogin)

test.test('labels', async ({ page }) => {
  // Empty labels panel
  await test.expect(actions.locateLabelsPanel(page)).toBeVisible()

  // "Create label" modal
  await actions.locateNewLabelButton(page).click()
  await test.expect(actions.locateNewLabelModal(page)).toBeVisible()
  await page.press('body', 'Escape')
  await test.expect(actions.locateNewLabelModal(page)).not.toBeVisible()
  await actions.locateNewLabelButton(page).click()
  await actions.locateModalBackground(page).click()
  await test.expect(actions.locateNewLabelModal(page)).not.toBeVisible()
  await actions.locateNewLabelButton(page).click()

  // "Create label" modal with name set
  await actions.locateNewLabelModalNameInput(page).fill('New Label')
  await test.expect(actions.locateNewLabelModal(page)).toHaveText(/^New Label/)

  await page.press('body', 'Escape')

  // "Create label" modal with color set
  // The exact number is allowed to vary; but to click the fourth color, there must be at least
  // four colors.
  await actions.locateNewLabelButton(page).click()
  test.expect(await actions.locateNewLabelModalColorButtons(page).count()).toBeGreaterThanOrEqual(4)
  // `force: true` is required because the `label` needs to handle the click event, not the
  // `button`.
  await actions.locateNewLabelModalColorButtons(page).nth(4).click({ force: true })
  await test.expect(actions.locateNewLabelModal(page)).toBeVisible()

  // "Create label" modal with name and color set
  await actions.locateNewLabelModalNameInput(page).fill('New Label')
  await test.expect(actions.locateNewLabelModal(page)).toHaveText(/^New Label/)

  // Labels panel with one entry
  await actions.locateCreateButton(actions.locateNewLabelModal(page)).click()
  await test.expect(actions.locateLabelsPanel(page)).toBeVisible()

  // Empty labels panel again, after deleting the only entry
  await actions.locateLabelsPanelLabels(page).first().hover()
  await actions.locateDeleteIcon(actions.locateLabelsPanel(page)).first().click()
  await actions.locateDeleteButton(page).click()
  test.expect(await actions.locateLabelsPanelLabels(page).count()).toBeGreaterThanOrEqual(1)
})
