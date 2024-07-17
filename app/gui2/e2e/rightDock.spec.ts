import { expect, test } from 'playwright/test'
import * as actions from './actions'
import { CONTROL_KEY } from './keyboard'
import * as locate from './locate'

test('Main method documentation', async ({ page }) => {
  await actions.goToGraph(page)

  // Documentation panel hotkey opens right-dock.
  await expect(locate.rightDock(page)).toBeHidden()
  await page.keyboard.press(`${CONTROL_KEY}+D`)
  await expect(locate.rightDock(page)).toBeVisible()

  // Right-dock displays main method documentation.
  await expect(locate.lexicalContent(locate.rightDock(page))).toHaveText('The main method')

  // Documentation hotkey closes right-dock.p
  await page.keyboard.press(`${CONTROL_KEY}+D`)
  await expect(locate.rightDock(page)).toBeHidden()
})
