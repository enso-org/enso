import { test, type Page } from '@playwright/test'
import * as actions from './actions'
import { expect } from './customExpect'
import { mockCollapsedFunctionInfo } from './expressionUpdates'
import { CONTROL_KEY } from './keyboard'
import * as locate from './locate'
import { mockSuggestion } from './suggestionUpdates'

const MAIN_FILE_NODES = 12

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
  await expect(locate.navBreadcrumb(page).and(page.locator('.inactive'))).toHaveText([
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
    .locator('.grab-handle')
    .click({ modifiers: ['Shift'] })
  await locate
    .graphNodeByBinding(page, 'sum')
    .locator('.grab-handle')
    .click({ modifiers: ['Shift'] })
  await locate
    .graphNodeByBinding(page, 'ten')
    .locator('.grab-handle')
    .click({ modifiers: ['Shift'] })

  await page.getByLabel('Group Selected Components').click()
  await expect(locate.graphNode(page)).toHaveCount(initialNodesCount - 2)
  await mockCollapsedFunctionInfo(page, 'prod', 'collapsed')
  await mockSuggestion(page, {
    type: 'method',
    module: 'local.Mock_Project',
    name: 'collapsed',
    isStatic: true,
    arguments: [{ name: 'five', reprType: 'Any', isSuspended: false, hasDefault: false }],
    selfType: 'local.Mock_Project',
    returnType: 'Standard.Base.Any.Any',
    annotations: [],
  })
  const collapsedNode = locate.graphNodeByBinding(page, 'prod')
  await expect(collapsedNode.locator('.WidgetFunctionName')).toExist()
  await expect(collapsedNode.locator('.WidgetFunctionName .WidgetToken')).toHaveText(['Main', '.'])
  await expect(collapsedNode.locator('.WidgetFunctionName input')).toHaveValue('collapsed')
  await expect(collapsedNode.locator('.WidgetTopLevelArgument')).toHaveText('five')

  await locate.graphNodeIcon(collapsedNode).dblclick()
  await expect(locate.graphNode(page)).toHaveCount(5)
  await expect(locate.inputNode(page)).toHaveCount(1)
  await expect(locate.graphNodeByBinding(page, 'ten')).toExist()
  await expect(locate.graphNodeByBinding(page, 'sum')).toExist()
  await expect(locate.graphNodeByBinding(page, 'prod')).toExist()
  await locate
    .graphNodeByBinding(page, 'ten')
    .locator('.grab-handle')
    .click({ modifiers: ['Shift'] })
  // Wait till node is selected.
  await expect(locate.graphNodeByBinding(page, 'ten').and(page.locator('.selected'))).toHaveCount(1)
  await page.keyboard.press(COLLAPSE_SHORTCUT)
  await expect(locate.graphNode(page)).toHaveCount(5)
  await expect(locate.inputNode(page)).toHaveCount(1)

  const secondCollapsedNode = locate.graphNodeByBinding(page, 'ten')
  await expect(secondCollapsedNode.locator('.WidgetToken')).toHaveText(['Main', '.', 'collapsed1'])
  await mockCollapsedFunctionInfo(page, 'ten', 'collapsed1')
  await secondCollapsedNode.dblclick()
  await expect(locate.graphNode(page)).toHaveCount(2)
  await expect(locate.graphNodeByBinding(page, 'ten')).toExist()
})

test('Input node', async ({ page }) => {
  await actions.goToGraph(page)
  await enterToFunc2(page)

  const inputNode = locate.inputNode(page)
  await expect(inputNode).toHaveCount(1)
  // Input node with identifier should have the icon and an identifier.
  await expect(inputNode.locator('.WidgetIcon')).toHaveCount(1)
  await expect(inputNode.locator('.WidgetToken')).toContainText('a')

  await inputNode.click()
  await page.keyboard.press('Delete')
  await expect(inputNode).toHaveCount(1)
  await inputNode.locator('.More').click({})
  await expect(inputNode.getByTestId('removeNode')).toHaveClass(/(?<=^| )disabled(?=$| )/)
})

test('Output node', async ({ page }) => {
  await actions.goToGraph(page)
  await enterToFunc2(page)

  const outputNode = locate.outputNode(page)
  await expect(outputNode).toHaveCount(1)
  // Output node with identifier should have only icon and no displayed identifiers
  await expect(outputNode.locator('.WidgetIcon')).toHaveCount(1)
  await expect(outputNode.locator('.WidgetToken')).toHaveCount(0)

  await outputNode.click()
  await page.keyboard.press('Delete')
  await expect(outputNode).toHaveCount(1)
  await outputNode.locator('.More').click({})
  await expect(outputNode.getByTestId('removeNode')).toHaveClass(/(?<=^| )disabled(?=$| )/)
})

test('Output node is not collapsed', async ({ page }) => {
  await actions.goToGraph(page)
  await enterToFunc2(page)

  await locate.outputNode(page).click({ modifiers: ['Shift'] })
  await locate
    .graphNodeByBinding(page, 'r')
    .locator('.grab-handle')
    .click({ modifiers: ['Shift'] })

  await page.getByLabel('Group Selected Components').click()
  await expect(locate.graphNodeByBinding(page, 'r').locator('.WidgetToken')).toHaveText([
    'Main',
    '.',
    'collapsed',
    'a',
  ])
  await expect(locate.inputNode(page)).toHaveCount(1)
})

test('Input node is not collapsed', async ({ page }) => {
  await actions.goToGraph(page)
  await enterToFunc2(page)

  await locate
    .graphNodeByBinding(page, 'r')
    .locator('.grab-handle')
    .click({ modifiers: ['Shift'] })
  await locate.inputNode(page).click({ modifiers: ['Shift'] })

  await page.getByLabel('Group Selected Components').click()
  await expect(locate.graphNodeByBinding(page, 'r').locator('.WidgetToken')).toHaveText([
    'Main',
    '.',
    'collapsed',
    'a',
  ])
  await expect(locate.outputNode(page)).toHaveCount(1)
})

async function expectInsideMain(page: Page) {
  await actions.expectNodePositionsInitialized(page, -16)
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
  await expect(locate.graphNodeByBinding(page, 'autoscoped')).toExist()
}

async function expectInsideFunc1(page: Page) {
  await actions.expectNodePositionsInitialized(page, -88)
  await expect(locate.graphNode(page)).toHaveCount(4)
  await expect(locate.inputNode(page)).toHaveCount(1)
  await expect(locate.graphNodeByBinding(page, 'f2')).toExist()
  await expect(locate.graphNodeByBinding(page, 'result')).toExist()
  await expect(locate.outputNode(page)).toHaveCount(1)
}

async function expectInsideFunc2(page: Page) {
  await actions.expectNodePositionsInitialized(page, -88)
  await expect(locate.graphNode(page)).toHaveCount(3)
  await expect(locate.inputNode(page)).toHaveCount(1)
  await expect(locate.graphNodeByBinding(page, 'r')).toExist()
  await expect(locate.outputNode(page)).toHaveCount(1)
}

async function enterToFunc2(page: Page) {
  await mockCollapsedFunctionInfo(page, 'final', 'func1')
  await locate.graphNodeByBinding(page, 'final').dblclick()
  await expectInsideFunc1(page)
  await mockCollapsedFunctionInfo(page, 'f2', 'func2')
  await locate.graphNodeByBinding(page, 'f2').dblclick()
  await expectInsideFunc2(page)
}
