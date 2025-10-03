import { test, type Page } from 'playwright/test'
import * as actions from './actions'
import { expect } from './customExpect'
import { mockUserDefinedFunctionInfo } from './expressionUpdates'
import { CONTROL_KEY, DELETE_KEY } from './keyboard'
import * as locate from './locate'
import { edgesFromNode, edgesToNode } from './locate'
import { mockSuggestion } from './suggestionUpdates'

const MAIN_FILE_NODES = 14
const EDGE_PARTS = 2

const COLLAPSE_SHORTCUT = `${CONTROL_KEY}+G`

test('Entering nodes', async ({ page }) => {
  await actions.goToGraph(page)
  await mockUserDefinedFunctionInfo(page, 'final', 'func1')
  await expectInsideMain(page)
  await expect(locate.navBreadcrumb(page)).toHaveText(['Mock Project'])

  await locate.graphNodeByBinding(page, 'final').dblclick()
  await expectInsideFunc1(page)
  await mockUserDefinedFunctionInfo(page, 'f2', 'func2')
  await expect(locate.navBreadcrumb(page)).toHaveText(['Mock Project', 'func1'])

  await locate.graphNodeByBinding(page, 'f2').dblclick()
  await expectInsideFunc2(page)
  await expect(locate.navBreadcrumb(page)).toHaveText(['Mock Project', 'func1', 'func2'])
})

test('Entering component shows error when function cannot be found (#12533)', async ({ page }) => {
  await actions.goToGraph(page)
  await mockUserDefinedFunctionInfo(page, 'final', 'no_such_func')
  await expectInsideMain(page)
  await expect(locate.navBreadcrumb(page)).toHaveText(['Mock Project'])
  await locate.graphNodeByBinding(page, 'final').dblclick()
  await expect(locate.navBreadcrumb(page)).toHaveText(['Mock Project', 'no_such_func'])
  await expect(page.locator('.GraphMissingView')).toExist()
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

function grabNode(page: Page, binding: string) {
  // Widgets may "steal" clicks, so we always click at icon.
  return locate.graphNodeByBinding(page, binding).locator('.grab-handle')
}

test.describe('Collapsing nodes with multiple inputs', () => {
  interface RelativePos {
    relativeTo?: string
    x: number
    y: number
  }

  function moveNode(page: Page, binding: string, relativePos: RelativePos) {
    return grabNode(page, binding).dragTo(grabNode(page, relativePos.relativeTo ?? binding), {
      force: true,
      targetPosition: relativePos,
    })
  }

  interface InputsPlacementTestCase {
    positions: { binding: string; relativePos: RelativePos }[]
    expectedOrder: string[]
    description: string
  }

  const inputPlacementTestCases: InputsPlacementTestCase[] = [
    // Default placement, five, ten and twenty are positioned vertically top-to-bottom.
    {
      description: 'Default placement',
      positions: [],
      expectedOrder: ['five', 'ten', 'twenty'],
    },
    // If horizontal position is the same, vertical position determines the order.
    // `ten` is moved up so it's on top of `five`.
    {
      description: 'Vertical alignment',
      positions: [{ binding: 'ten', relativePos: { x: 0, y: -160 } }],
      expectedOrder: ['ten', 'five', 'twenty'],
    },
    // Horizontal position determines the order when vertical position is similar.
    // `ten` is moved to the right of `five`, `twenty` is moved to the left of `five`.
    {
      description: 'Horizontal alignment',
      positions: [
        { binding: 'ten', relativePos: { relativeTo: 'five', x: 160, y: 0 } },
        { binding: 'twenty', relativePos: { relativeTo: 'five', x: -160, y: 0 } },
      ],
      expectedOrder: ['twenty', 'five', 'ten'],
    },
    // Horizontal position determines the order even when vertical position is different.
    // `ten` is moved to the left, `five` is moved to the right.
    {
      description: 'Left-to-right positioning with different vertical position',
      positions: [
        { binding: 'ten', relativePos: { relativeTo: 'five', x: -160, y: 0 } },
        { binding: 'five', relativePos: { x: 160, y: 0 } },
      ],
      expectedOrder: ['ten', 'twenty', 'five'],
    },
  ]

  inputPlacementTestCases.forEach((testCase) => {
    test(`${testCase.description}`, async ({ page }) => {
      await actions.goToGraph(page)
      const initialNodesCount = await locate.graphNode(page).count()
      await mockUserDefinedFunctionInfo(page, 'final', 'func1')

      for (const { binding, relativePos } of testCase.positions) {
        await moveNode(page, binding, relativePos)
      }

      // Reset selection
      await page.mouse.click(250, 300)
      await expect(locate.graphNode(page).locator('.selected')).toHaveCount(0)

      await grabNode(page, 'sum').click({ modifiers: ['Shift'] })
      await grabNode(page, 'prod').click({ modifiers: ['Shift'] })

      await page.getByTestId('action:components.collapse').click()
      await expect(locate.graphNode(page)).toHaveCount(initialNodesCount - 1)
      await mockUserDefinedFunctionInfo(page, 'prod', 'user_defined_component')
      const collapsedNode = locate.graphNodeByBinding(page, 'prod')
      await expect(collapsedNode.locator('.WidgetApplication.prefix > .WidgetPort')).toHaveText(
        'Main.user_defined_component',
      )

      // Enter collapsed node
      await locate.graphNodeIcon(collapsedNode).dblclick()
      await expect(locate.graphNode(page)).toHaveCount(6)
      await expect(locate.inputNode(page)).toHaveCount(3)

      // Check input nodes are positioned in the correct order
      await expectInputNodesInOrder(page, testCase.expectedOrder)
    })
  })
})

async function expectInputNodesInOrder(page: Page, expectedOrder: string[]) {
  const inputNodes = await locate.inputNode(page).all()
  const inputNodePositions = await Promise.all(
    inputNodes.map(async (node) => {
      const nodeText = (await node.locator('.WidgetToken').allTextContents())[0]
      const bbox = await node.boundingBox()
      expect(nodeText).toBeDefined()
      expect(bbox).toBeDefined()
      return { text: nodeText!, bbox: bbox! }
    }),
  )
  // Check that all input nodes have the same y coordinate
  expect(inputNodePositions.length).toBe(expectedOrder.length)
  const yCoords = inputNodePositions.map((pos) => pos.bbox.y)
  expect(new Set(yCoords).size).toBe(1)
  // Check that nodes are arranged left-to-right in the expected order
  const actualOrder = inputNodePositions
    .slice()
    .sort((a, b) => a.bbox.x - b.bbox.x)
    .map((pos) => pos.text)

  expect(actualOrder).toEqual(expectedOrder)
}

test('Collapsing nodes', async ({ page }) => {
  await actions.goToGraph(page)
  const initialNodesCount = await locate.graphNode(page).count()
  await mockUserDefinedFunctionInfo(page, 'final', 'func1')

  await grabNode(page, 'prod').click({ modifiers: ['Shift'] })
  await grabNode(page, 'sum').click({ modifiers: ['Shift'] })
  await grabNode(page, 'ten').click({ modifiers: ['Shift'] })
  await grabNode(page, 'twenty').click({ modifiers: ['Shift'] })

  await page.getByTestId('action:components.collapse').click()
  await expect(locate.graphNode(page)).toHaveCount(initialNodesCount - 3)
  await mockUserDefinedFunctionInfo(page, 'prod', 'user_defined_component')
  await mockSuggestion(page, {
    type: 'method',
    module: 'local.Mock_Project',
    name: 'user_defined_component',
    isStatic: true,
    arguments: [{ name: 'five', reprType: 'Any', isSuspended: false, hasDefault: false }],
    selfType: 'local.Mock_Project',
    returnType: 'Standard.Base.Any.Any',
    annotations: [],
  })
  const collapsedNode = locate.graphNodeByBinding(page, 'prod')
  await expect(collapsedNode.locator('.WidgetApplication.prefix > .WidgetPort')).toExist()
  await expect(collapsedNode.locator('.WidgetApplication.prefix > .WidgetPort')).toHaveText(
    'Main.user_defined_component',
  )
  await expect(collapsedNode.locator('.WidgetTopLevelArgument')).toHaveText('five')

  await locate.graphNodeIcon(collapsedNode).dblclick()
  await expect(locate.graphNode(page)).toHaveCount(6)
  await expect(locate.inputNode(page)).toHaveCount(1)
  await expect(locate.graphNodeByBinding(page, 'ten')).toExist()
  await expect(locate.graphNodeByBinding(page, 'sum')).toExist()
  await expect(locate.graphNodeByBinding(page, 'prod')).toExist()
  await grabNode(page, 'ten').click({ modifiers: ['Shift'] })
  await grabNode(page, 'sum').click({ modifiers: ['Shift'] })
  // Wait till node is selected.
  await expect(locate.graphNodeByBinding(page, 'sum').and(page.locator('.selected'))).toHaveCount(1)
  await page.keyboard.press(COLLAPSE_SHORTCUT)
  await expect(locate.graphNode(page)).toHaveCount(5)
  await expect(locate.inputNode(page)).toHaveCount(1)

  const secondCollapsedNode = locate.graphNodeByBinding(page, 'sum')
  await expect(secondCollapsedNode.locator('.WidgetToken')).toHaveText([
    'Main',
    '.',
    'user_defined_component1',
    'five',
    'twenty',
  ])
  await mockUserDefinedFunctionInfo(page, 'sum', 'user_defined_component1')
  await secondCollapsedNode.dblclick()
  await expect(locate.graphNodeByBinding(page, 'ten')).toExist()
  await expect(locate.graphNode(page)).toHaveCount(5)
})

test('Display message when User Defined Component ceases to exist', async ({ page }) => {
  await actions.goToGraph(page)

  const initialNodesCount = await locate.graphNode(page).count()
  await grabNode(page, 'prod').click({ modifiers: ['Shift'] })
  await grabNode(page, 'sum').click({ modifiers: ['Shift'] })
  await page.getByTestId('action:components.collapse').click()
  await expect(locate.graphNode(page)).toHaveCount(initialNodesCount - 1)
  await mockUserDefinedFunctionInfo(page, 'prod', 'user_defined_component')

  const collapsedNode = locate.graphNodeByBinding(page, 'prod')
  await locate.graphNodeIcon(collapsedNode).dblclick()
  await expect(locate.inputNode(page)).toExist()

  await page.keyboard.press(`${CONTROL_KEY}+Z`)
  await expect(page.locator('.GraphMissingView')).toExist()
})

test('Input node', async ({ page }) => {
  await actions.goToGraph(page)
  await enterToFunc2(page)

  const inputNode = locate.inputNode(page)
  await expect(inputNode).toHaveCount(1)

  // Input node with identifier should have the icon and an identifier.
  await expect(inputNode.locator('.WidgetIcon')).toHaveCount(1)
  await expect(inputNode.locator('.WidgetToken')).toContainText('a')

  // Input node has output port
  const outputPort = await locate.outputPortCoordinates(page, inputNode)
  await page.mouse.click(outputPort.x + 20, outputPort.y)
  await locate.graphEditor(page).click({ position: { x: 100, y: 500 } })
  await expect(locate.componentBrowser(page)).toExist()
  await page.keyboard.press('Escape')

  // Input node cannot be deleted
  await inputNode.click()
  await page.keyboard.press('Delete')
  await expect(inputNode).toHaveCount(1)
  await inputNode.locator('.More').click({})
  await expect(inputNode.getByTestId('action:components.deleteSelected')).toHaveClass(
    /(?<=^| )disabled(?=$| )/,
  )
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
  await expect(outputNode.getByTestId('action:components.deleteSelected')).toHaveClass(
    /(?<=^| )disabled(?=$| )/,
  )
})

test('Output node is not collapsed', async ({ page }) => {
  await actions.goToGraph(page)
  await enterToFunc2(page)

  await locate.outputNode(page).click({ modifiers: ['Shift'] })
  await grabNode(page, 'r').click({ modifiers: ['Shift'] })

  await page.getByTestId('action:components.collapse').click()
  await expect(locate.graphNodeByBinding(page, 'r').locator('.WidgetToken')).toHaveText([
    'Main',
    '.',
    'user_defined_component',
    'a',
  ])
  await expect(locate.inputNode(page)).toHaveCount(1)
})

test('Input node is not collapsed', async ({ page }) => {
  await actions.goToGraph(page)
  await enterToFunc2(page)

  await grabNode(page, 'r').click({ modifiers: ['Shift'] })
  await locate.inputNode(page).click({ modifiers: ['Shift'] })

  await page.getByTestId('action:components.collapse').click()
  await expect(locate.graphNodeByBinding(page, 'r').locator('.WidgetToken')).toHaveText([
    'Main',
    '.',
    'user_defined_component',
    'a',
  ])
  await expect(locate.outputNode(page)).toHaveCount(1)
})

test('User Defined Component call shows argument placeholders', async ({ page }) => {
  await actions.goToGraph(page)
  await mockUserDefinedFunctionInfo(page, 'final', 'func1', [0])
  await mockSuggestion(page, {
    type: 'method',
    module: 'local.Mock_Project.Main',
    name: 'func1',
    arguments: [
      {
        name: 'arg1',
        reprType: 'Standard.Base.Any.Any',
        isSuspended: false,
        hasDefault: false,
        defaultValue: null as any,
        tagValues: null as any,
      },
    ],
    selfType: 'local.Mock_Project.Main',
    returnType: 'Standard.Base.Any.Any',
    isStatic: true,
    documentation: '',
    annotations: [],
  })
  const collapsedCallComponent = locate.graphNodeByBinding(page, 'final')
  await locate.graphNodeByBinding(page, 'prod').click()
  await page.keyboard.press(DELETE_KEY)
  await expect(await edgesToNode(page, collapsedCallComponent)).toHaveCount(0)
  await expect(locate.selectedNodes(page)).toHaveCount(0)
  await expect(collapsedCallComponent.locator('.WidgetArgumentName .name')).toHaveText('arg1')
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
  // The mouse is often in output port area, making our checks fooled by the edge ghost.
  await page.mouse.move(0, 0)
  await actions.expectNodePositionsInitialized(page, -88)
  await expect(locate.graphNode(page)).toHaveCount(4)
  await expect(locate.inputNode(page)).toHaveCount(1)
  await expect(locate.graphNodeByBinding(page, 'f2')).toExist()
  await expect(locate.graphNodeByBinding(page, 'result')).toExist()
  await expect(locate.outputNode(page)).toHaveCount(1)
  await expect(await edgesFromNode(page, locate.inputNode(page))).toHaveCount(EDGE_PARTS)
  await expect(await edgesToNode(page, locate.outputNode(page))).toHaveCount(EDGE_PARTS)
}

async function expectInsideFunc2(page: Page) {
  // The mouse is often in input's output port area, making our checks fooled by the edge ghost.
  await page.mouse.move(0, 0)
  await actions.expectNodePositionsInitialized(page, -88)
  await expect(locate.graphNode(page)).toHaveCount(3)
  await expect(locate.inputNode(page)).toHaveCount(1)
  await expect(locate.graphNodeByBinding(page, 'r')).toExist()
  await expect(locate.outputNode(page)).toHaveCount(1)
  await expect(await edgesFromNode(page, locate.inputNode(page))).toHaveCount(EDGE_PARTS)
  await expect(await edgesToNode(page, locate.outputNode(page))).toHaveCount(EDGE_PARTS)
}

async function enterToFunc2(page: Page) {
  await mockUserDefinedFunctionInfo(page, 'final', 'func1')
  await locate.graphNodeByBinding(page, 'final').dblclick()
  await expectInsideFunc1(page)
  await mockUserDefinedFunctionInfo(page, 'f2', 'func2')
  await locate.graphNodeByBinding(page, 'f2').dblclick()
  await expectInsideFunc2(page)
}
