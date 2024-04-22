import { test, type Page } from '@playwright/test'
import * as actions from './actions'
import { expect } from './customExpect'
import { mockCollapsedFunctionInfo } from './expressionUpdates'
import { CONTROL_KEY } from './keyboard'
import * as locate from './locate'

const MAIN_FILE_NODES = 11

const COLLAPSE_SHORTCUT = `${CONTROL_KEY}+G`

test('Entering nodes', async ({ page }) => {
  await actions.goToGraph(page)
  await mockCollapsedFunctionInfo(page, 'final', 'func1')
  await expectInsideMain(page)
  await expect(locate.navBreadcrumb(page)).toHaveText(['Mock Project'])

  await locate.graphNodeByBinding(page, 'final').dblclick()
  await mockCollapsedFunctionInfo(page, 'f2', 'func2')
  await expectInsideFunc1(page)
  await expect(locate.navBreadcrumb(page)).toHaveText(['Mock Project', 'func1'])

  await locate.graphNodeByBinding(page, 'f2').dblclick()
  await expectInsideFunc2(page)
  await expect(locate.navBreadcrumb(page)).toHaveText(['Mock Project', 'func1', 'func2'])
})

test('Leaving entered nodes', async ({ page }) => {
  await actions.goToGraph(page)
  await enterToFunc2(page)

  await actions.exitFunction(page)
  await expectInsideFunc1(page)

  await actions.exitFunction(page)
  await expectInsideMain(page)
})

test('Using breadcrumbs to navigate', async ({ page }) => {
  await actions.goToGraph(page)
  await enterToFunc2(page)
  await actions.exitFunction(page)
  await expectInsideFunc1(page)
  await actions.exitFunction(page)
  await expectInsideMain(page)
  // Breadcrumbs still have all the crumbs, but the last two are dimmed.
  await expect(locate.navBreadcrumb(page)).toHaveText(['Mock Project', 'func1', 'func2'])
  await expect(locate.navBreadcrumb(page, (f) => f.class('inactive'))).toHaveText([
    'func1',
    'func2',
  ])

  await locate.navBreadcrumb(page).filter({ hasText: 'func2' }).click()
  await expectInsideFunc2(page)

  await locate.navBreadcrumb(page).filter({ hasText: 'Mock Project' }).click()
  await expectInsideMain(page)

  await locate.navBreadcrumb(page).filter({ hasText: 'func1' }).click()
  await expectInsideFunc1(page)
})

test('Collapsing nodes', async ({ page }) => {
  await actions.goToGraph(page)
  const initialNodesCount = await locate.graphNode(page).count()
  await mockCollapsedFunctionInfo(page, 'final', 'func1')

  // Widgets may "steal" clicks, so we always click at icon.
  await locate
    .graphNodeByBinding(page, 'prod')
    .locator('.icon')
    .click({ modifiers: ['Shift'] })
  await locate
    .graphNodeByBinding(page, 'sum')
    .locator('.icon')
    .click({ modifiers: ['Shift'] })
  await locate
    .graphNodeByBinding(page, 'ten')
    .locator('.icon')
    .click({ modifiers: ['Shift'] })

  await page.keyboard.press(COLLAPSE_SHORTCUT)
  await expect(locate.graphNode(page)).toHaveCount(initialNodesCount - 2)
  const collapsedNode = locate.graphNodeByBinding(page, 'prod')
  await expect(collapsedNode.locator('.WidgetToken')).toHaveText(['Main', '.', 'collapsed', 'five'])
  await mockCollapsedFunctionInfo(page, 'prod', 'collapsed')

  await collapsedNode.dblclick()
  await expect(locate.graphNode(page)).toHaveCount(4)
  await expect(locate.graphNodeByBinding(page, 'ten')).toExist()
  await expect(locate.graphNodeByBinding(page, 'sum')).toExist()
  await expect(locate.graphNodeByBinding(page, 'prod')).toExist()

  await locate
    .graphNodeByBinding(page, 'ten')
    .locator('.icon')
    .click({ modifiers: ['Shift'] })
  // Wait till node is selected.
  await expect(locate.graphNodeByBinding(page, 'ten').and(page.locator('.selected'))).toHaveCount(1)
  await page.keyboard.press(COLLAPSE_SHORTCUT)
  await expect(locate.graphNode(page)).toHaveCount(4)

  const secondCollapsedNode = locate.graphNodeByBinding(page, 'ten')
  await expect(secondCollapsedNode.locator('.WidgetToken')).toHaveText(['Main', '.', 'collapsed1'])
  await mockCollapsedFunctionInfo(page, 'ten', 'collapsed1')
  await secondCollapsedNode.dblclick()
  await expect(locate.graphNode(page)).toHaveCount(2)
  await expect(locate.graphNodeByBinding(page, 'ten')).toExist()
})

async function expectInsideMain(page: Page) {
  await actions.expectNodePositionsInitialized(page, 64)
  await expect(locate.graphNode(page)).toHaveCount(MAIN_FILE_NODES)
  await expect(locate.graphNodeByBinding(page, 'five')).toExist()
  await expect(locate.graphNodeByBinding(page, 'ten')).toExist()
  await expect(locate.graphNodeByBinding(page, 'sum')).toExist()
  await expect(locate.graphNodeByBinding(page, 'prod')).toExist()
  await expect(locate.graphNodeByBinding(page, 'final')).toExist()
  await expect(locate.graphNodeByBinding(page, 'list')).toExist()
  await expect(locate.graphNodeByBinding(page, 'data')).toExist()
  await expect(locate.graphNodeByBinding(page, 'aggregated')).toExist()
  await expect(locate.graphNodeByBinding(page, 'filtered')).toExist()
}

async function expectInsideFunc1(page: Page) {
  await actions.expectNodePositionsInitialized(page, 192)
  await expect(locate.graphNode(page)).toHaveCount(3)
  await expect(locate.graphNodeByBinding(page, 'f2')).toExist()
  await expect(locate.graphNodeByBinding(page, 'result')).toExist()
}

async function expectInsideFunc2(page: Page) {
  await actions.expectNodePositionsInitialized(page, 128)
  await expect(locate.graphNode(page)).toHaveCount(2)
  await expect(locate.graphNodeByBinding(page, 'r')).toExist()
}

async function enterToFunc2(page: Page) {
  await mockCollapsedFunctionInfo(page, 'final', 'func1')
  await locate.graphNodeByBinding(page, 'final').dblclick()
  await expectInsideFunc1(page)
  await mockCollapsedFunctionInfo(page, 'f2', 'func2')
  await locate.graphNodeByBinding(page, 'f2').dblclick()
  await expectInsideFunc2(page)
}
