import { expect, test } from 'playwright/test'
import * as actions from './actions'
import { mockMethodCallInfo } from './expressionUpdates'
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

test('Doc panel focus (regression #10471)', async ({ page }) => {
  await actions.goToGraph(page)

  await page.keyboard.press(`${CONTROL_KEY}+D`)
  await page.keyboard.press(`${CONTROL_KEY}+\``)
  await expect(locate.rightDock(page)).toBeVisible()
  await expect(locate.bottomDock(page)).toBeVisible()

  // Focus code editor.
  await locate.bottomDock(page).click()

  await page.evaluate(() => {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const codeEditor = (window as any).__codeEditorApi
    const docStart = codeEditor.indexOf('The main method')
    codeEditor.placeCursor(docStart + 8)
  })
  await page.keyboard.press('Space')
  await page.keyboard.press('T')
  await page.keyboard.press('E')
  await page.keyboard.press('S')
  await page.keyboard.press('T')

  const content = await page.evaluate(() => {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const codeEditor = (window as any).__codeEditorApi
    return codeEditor.textContent()
  })
  expect(content.includes('The main TEST method')).toBe(true)
  await expect(locate.rightDock(page)).toContainText('The main TEST method')
})

test('Component help', async ({ page }) => {
  await actions.goToGraph(page, false)
  await locate.rightDock(page).getByRole('button', { name: 'Help' }).click()
  await expect(locate.rightDock(page)).toHaveText(/Select a single component/)

  await locate.graphNodeByBinding(page, 'final').click()
  await expect(locate.rightDock(page)).toHaveText(/No documentation available/)

  await mockMethodCallInfo(page, 'data', {
    methodPointer: {
      module: 'Standard.Base.Data',
      definedOnType: 'Standard.Base.Data',
      name: 'read',
    },
    notAppliedArguments: [0, 1, 2],
  })
  await locate.graphNodeByBinding(page, 'data').click()
  await expect(locate.rightDock(page)).toHaveText(/Reads a file into Enso/)
})
