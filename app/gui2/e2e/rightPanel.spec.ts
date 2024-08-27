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

test('Show component help setting', async ({ page }) => {
  await actions.goToGraph(page, false)
  await locate.rightDock(page).getByRole('button', { name: 'Help' }).click()

  const openCB = async () => {
    await locate.addNewNodeButton(page).click()
    await expect(locate.componentBrowser(page)).toExist()
    await expect(locate.componentBrowserEntry(page)).toExist()
  }
  const closeCB = async () => {
    await page.keyboard.press('Escape')
    await expect(locate.componentBrowser(page)).toBeHidden()
  }
  const toggleDocPanel = async () => {
    await locate.rightDockRoot(page).locator('.toggleDock').click()
  }
  const expectDocPanelHidden = async () => {
    await expect(locate.rightDock(page)).toBeHidden()
  }
  const expectDocPanelShown = async () => {
    await expect(locate.rightDock(page)).toBeVisible()
  }

  await page.keyboard.press('Escape')
  await expect(locate.selectedNodes(page)).toHaveCount(0)

  await expectDocPanelShown()
  await expect(locate.rightDock(page)).toHaveText(/Select a single component/)
  await openCB()
  await expectDocPanelShown()
  await expect(locate.rightDock(page)).toHaveText(/Reads a file into Enso/)
  await closeCB()
  await expectDocPanelShown()
  await expect(locate.rightDock(page)).toHaveText(/Select a single component/)

  await toggleDocPanel()
  await expectDocPanelHidden()
  await openCB()
  await expectDocPanelShown()
  await closeCB()
  await expectDocPanelHidden()

  await openCB()
  await expectDocPanelShown()
  await toggleDocPanel()
  await expectDocPanelHidden()
  await closeCB()
  await expectDocPanelHidden()
  await toggleDocPanel()
  await expectDocPanelShown()

  await openCB()
  await expectDocPanelShown()
  await closeCB()
  await expectDocPanelShown()
})
