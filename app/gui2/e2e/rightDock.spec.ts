import { expect, test } from 'playwright/test'
import * as actions from './actions'
import { CONTROL_KEY } from './keyboard'

test('Main method documentation', async ({ page }) => {
  await actions.goToGraph(page)

  // Documentation panel hotkey opens right-dock.
  await expect(page.getByTestId('rightDock')).not.toBeVisible()
  await page.keyboard.press(`${CONTROL_KEY}+D`)
  await expect(page.getByTestId('rightDock')).toBeVisible()

  // Right-dock displays main method documentation.
  await expect(page.getByTestId('rightDock')).toHaveText('The main method')

  // Documentation hotkey closes right-dock.p
  await page.keyboard.press(`${CONTROL_KEY}+D`)
  await expect(page.getByTestId('rightDock')).not.toBeVisible()
})
