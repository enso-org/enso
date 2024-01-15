import { expect, test, type Page } from '@playwright/test'
import os from 'os'
import * as actions from './actions'
import * as customExpect from './customExpect'
import { mockCollapsedFunctionInfo } from './expressionUpdates'
import * as locate from './locate'

const COLLAPSE_SHORTCUT = os.platform() === 'darwin' ? 'Meta+G' : 'Control+G'

test('Entering nodes', async ({ page }) => {
  await actions.goToGraph(page)
  await mockCollapsedFunctionInfo(page, 'final', 'func1')
  await expectInsideMain(page)
  await expect(locate.navBreadcrumb(page)).toHaveText(['main'])

  await locate.graphNodeByBinding(page, 'final').dblclick()
  await mockCollapsedFunctionInfo(page, 'f2', 'func2')
  await expectInsideFunc1(page)
  await expect(locate.navBreadcrumb(page)).toHaveText(['main', 'func1'])

  await locate.graphNodeByBinding(page, 'f2').dblclick()
  await expectInsideFunc2(page)
  await expect(locate.navBreadcrumb(page)).toHaveText(['main', 'func1', 'func2'])
})

test('Leaving entered nodes', async ({ page }) => {
  await actions.goToGraph(page)
  await enterToFunc2(page)

  await locate.graphEditor(page).dblclick()
  await expectInsideFunc1(page)

  await locate.graphEditor(page).dblclick()
  await expectInsideMain(page)
})

test('Using breadcrumbs to navigate', async ({ page }) => {
  await actions.goToGraph(page)
  await enterToFunc2(page)
  await locate.graphEditor(page).dblclick()
  await locate.graphEditor(page).dblclick()
  // Breadcrumbs still have all the crumbs, but the last two are dimmed.
  await expect(locate.navBreadcrumb(page)).toHaveText(['main', 'func1', 'func2'])
  await expect(locate.navBreadcrumb(page, (f) => f.class('inactive'))).toHaveText([
    'func1',
    'func2',
  ])

  // TODO: This actually fails on develop. https://github.com/enso-org/enso/issues/8756
  //
  // await locate.navBreadcrumb(page).filter({ hasText: 'func2' }).click()
  // await isInsideFunc2(page)
  //
  // await locate.navBreadcrumb(page).filter({ hasText: 'main' }).click()
  // await isInsideMain(page)
  //
  // await locate.navBreadcrumb(page).filter({ hasText: 'func1' }).click()
  // await isInsideFunc1(page)
})

test('Collapsing nodes', async ({ page }) => {
  await actions.goToGraph(page)
  await mockCollapsedFunctionInfo(page, 'final', 'func1')

  await locate.graphNodeByBinding(page, 'ten').click({ modifiers: ['Shift'] })
  await locate.graphNodeByBinding(page, 'sum').click({ modifiers: ['Shift'] })
  await locate.graphNodeByBinding(page, 'prod').click({ modifiers: ['Shift'] })

  await page.keyboard.press(COLLAPSE_SHORTCUT)
  await expect(locate.graphNode(page)).toHaveCount(3)
  const collapsedNode = locate.graphNodeByBinding(page, 'prod')
  await expect(collapsedNode.locator('.WidgetToken')).toHaveText(['Main', '.', 'collapsed', 'five'])
  await mockCollapsedFunctionInfo(page, 'prod', 'collapsed')

  await collapsedNode.dblclick()
  await expect(locate.graphNode(page)).toHaveCount(4)
  await customExpect.toExist(locate.graphNodeByBinding(page, 'ten'))
  await customExpect.toExist(locate.graphNodeByBinding(page, 'sum'))
  await customExpect.toExist(locate.graphNodeByBinding(page, 'prod'))

  locate.graphNodeByBinding(page, 'ten').click({ modifiers: ['Shift'] })
  // Wait till node is selected.
  await expect(locate.graphNodeByBinding(page, 'ten').and(page.locator('.selected'))).toHaveCount(1)
  await page.keyboard.press(COLLAPSE_SHORTCUT)
  await expect(locate.graphNode(page)).toHaveCount(4)

  const secondCollapsedNode = locate.graphNodeByBinding(page, 'ten')
  await expect(secondCollapsedNode.locator('.WidgetToken')).toHaveText(['Main', '.', 'collapsed1'])
  await mockCollapsedFunctionInfo(page, 'ten', 'collapsed1')
  secondCollapsedNode.dblclick()
  await expect(locate.graphNode(page)).toHaveCount(2)
  await customExpect.toExist(locate.graphNodeByBinding(page, 'ten'))
})

async function expectInsideMain(page: Page) {
  await expect(locate.graphNode(page)).toHaveCount(5)
  await customExpect.toExist(locate.graphNodeByBinding(page, 'five'))
  await customExpect.toExist(locate.graphNodeByBinding(page, 'ten'))
  await customExpect.toExist(locate.graphNodeByBinding(page, 'sum'))
  await customExpect.toExist(locate.graphNodeByBinding(page, 'prod'))
  await customExpect.toExist(locate.graphNodeByBinding(page, 'final'))
}

async function expectInsideFunc1(page: Page) {
  await expect(locate.graphNode(page)).toHaveCount(3)
  await customExpect.toExist(locate.graphNodeByBinding(page, 'f2'))
  await customExpect.toExist(locate.graphNodeByBinding(page, 'result'))
}

async function expectInsideFunc2(page: Page) {
  await expect(locate.graphNode(page)).toHaveCount(2)
  await customExpect.toExist(locate.graphNodeByBinding(page, 'r'))
}

async function enterToFunc2(page: Page) {
  await mockCollapsedFunctionInfo(page, 'final', 'func1')
  await locate.graphNodeByBinding(page, 'final').dblclick()
  await mockCollapsedFunctionInfo(page, 'f2', 'func2')
  await locate.graphNodeByBinding(page, 'f2').dblclick()
}
