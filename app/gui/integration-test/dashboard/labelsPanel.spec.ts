/** @file Test the labels sidebar panel. */
import { expect, test, type Locator, type Page } from '@playwright/test'

import { mockAllAndLogin, TEXT } from './actions'

/** Find a "new label" button. */
function locateNewLabelButton(page: Page) {
  return page.getByRole('button', { name: 'new label' }).getByText('new label')
}

/** Find a labels panel. */
function locateLabelsPanel(page: Page) {
  // This has no identifying features.
  return page.getByTestId('labels')
}

/** Find a "new label" modal. */
function locateNewLabelModal(page: Page) {
  // This has no identifying features.
  return page.getByTestId('new-label-modal')
}

/** Find a "name" input for a "new label" modal. */
function locateNewLabelModalNameInput(page: Page) {
  return locateNewLabelModal(page).getByLabel('Name').and(page.getByRole('textbox'))
}

/** Find all color radio button inputs for a "new label" modal. */
function locateNewLabelModalColorButtons(page: Page) {
  return (
    locateNewLabelModal(page)
      .filter({ has: page.getByText('Color') })
      // The `radio` inputs are invisible, so they cannot be used in the locator.
      .locator('label[data-rac]')
  )
}

/** Find a "create" button. */
function locateCreateButton(page: Locator) {
  return page.getByRole('button', { name: TEXT.create }).getByText(TEXT.create)
}

/** Find all labels in the labels panel. */
function locateLabelsPanelLabels(page: Page, name?: string) {
  return (
    locateLabelsPanel(page)
      .getByRole('button')
      .filter(name != null ? { has: page.getByText(name) } : {})
      // The delete button is also a `button`.
      .and(page.locator(':nth-child(1)'))
  )
}

test('labels', ({ page }) =>
  mockAllAndLogin({ page })
    .do(async (page) => {
      // Empty labels panel
      await expect(locateLabelsPanel(page)).toBeVisible()

      // "New Label" modal
      await locateNewLabelButton(page).click()
      await expect(locateNewLabelModal(page)).toBeVisible()

      // "New Label" modal with name set
      await locateNewLabelModalNameInput(page).fill('New Label')
      await expect(locateNewLabelModal(page)).toHaveText(/^New Label/)
    })
    .press('Escape')
    .do(async (page) => {
      // "New Label" modal with color set
      // The exact number is allowed to vary; but to click the fourth color, there must be at least
      // four colors.
      await locateNewLabelButton(page).click()
      expect(await locateNewLabelModalColorButtons(page).count()).toBeGreaterThanOrEqual(4)
      // `force: true` is required because the `label` needs to handle the click event, not the
      // `button`.
      await locateNewLabelModalColorButtons(page).nth(4).click({ force: true })
      await expect(locateNewLabelModal(page)).toBeVisible()

      // "New Label" modal with name and color set
      await locateNewLabelModalNameInput(page).fill('New Label')
      await expect(locateNewLabelModal(page)).toHaveText(/^New Label/)

      // Labels panel with one entry
      await locateCreateButton(locateNewLabelModal(page)).click()
      await expect(locateLabelsPanel(page)).toBeVisible()
      expect(await locateLabelsPanelLabels(page).count()).toBe(1)

      // Empty labels panel again, after deleting the only entry
      await locateLabelsPanelLabels(page).first().hover()

      const labelsPanel = locateLabelsPanel(page)
      await labelsPanel.getByRole('button').and(labelsPanel.getByLabel(TEXT.delete)).click()
      await page.getByRole('button', { name: TEXT.delete }).getByText(TEXT.delete).click()
      expect(await locateLabelsPanelLabels(page).count()).toBe(0)
    }))
