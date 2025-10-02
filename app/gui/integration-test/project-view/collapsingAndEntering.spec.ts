import type EditorPageActions from 'integration-test/actions/EditorPageActions'
import type { RelativePos } from 'integration-test/actions/EditorPageActions'
import { expect, test } from 'integration-test/base'
import { DELETE_KEY } from './keyboard'
import * as locate from './locate'
import { edgesFromNode, edgesToNode } from './locate'

const MAIN_FILE_NODES = 14
const EDGE_PARTS = 2

const COLLAPSE_SHORTCUT = `Mod+G`

test('Entering nodes', async ({ editorPage }) => {
  await editorPage
    .mockUserDefinedFunctionInfo('final', 'func1')
    .call(expectInsideMain)
    .expectBreadcrumbs(['Mock Project'])
    .enterNode('final')
    .call(expectInsideFunc1)
    .mockUserDefinedFunctionInfo('f2', 'func2')
    .expectBreadcrumbs(['Mock Project', 'func1'])
    .enterNode('f2')
    .call(expectInsideFunc2)
    .expectBreadcrumbs(['Mock Project', 'func1', 'func2'])
})

test('Entering component shows error when function cannot be found (#12533)', async ({
  editorPage,
}) => {
  await editorPage
    .mockUserDefinedFunctionInfo('final', 'no_such_func')
    .call(expectInsideMain)
    .expectBreadcrumbs(['Mock Project'])
    .enterNode('final')
    .expectBreadcrumbs(['Mock Project', 'no_such_func'])
    .do((page) => expect(page.locator('.GraphMissingView')).toExist())
})

test('Leaving entered nodes', async ({ editorPage }) => {
  await editorPage
    .call(enterToFunc2)
    .exitFunction()
    .call(expectInsideFunc1)
    .exitFunction()
    .call(expectInsideMain)
})

test('Using breadcrumbs to navigate', async ({ editorPage }) => {
  await editorPage
    .call(enterToFunc2)
    .exitFunction()
    .exitFunction()
    // Breadcrumbs still have all the crumbs, but the last two are dimmed.
    .expectBreadcrumbs(['Mock Project', 'func1', 'func2'])
    .expectBreadcrumbs(['func1', 'func2'], '.inactive')
    .clickBreadcrumb('func2')
    .call(expectInsideFunc2)
    .clickBreadcrumb('Mock Project')
    .call(expectInsideMain)
    .clickBreadcrumb('func1')
    .call(expectInsideFunc1)
})

test.describe('Collapsing nodes with multiple inputs', () => {
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
    test(`${testCase.description}`, async ({ editorPage }) => {
      await editorPage.mockUserDefinedFunctionInfo('final', 'func1')
      const initialNodesCount = await editorPage.nodeCount()

      for (const { binding, relativePos } of testCase.positions) {
        await editorPage.dragNode(binding, relativePos)
      }

      await editorPage
        .clearSelection()
        .selectNodes(['sum', 'prod'])
        .clickActionTrigger('components.collapse')
        .mockUserDefinedFunctionInfo('final', 'func1')
        .expectNodeCount(initialNodesCount - 1)
        .mockUserDefinedFunctionInfo('prod', 'user_defined_component')
        .withNode('prod', async (node) => {
          await expect(node.locator('.WidgetApplication.prefix > .WidgetPort')).toHaveText(
            'Main.user_defined_component',
          )
        })
        .enterNode('prod')
        .expectNodeCount(6)
        .expectNodeCount(3, '.inputNode')
        .expectInputNodesInOrder(testCase.expectedOrder)
    })
  })
})

test('Collapsing nodes', async ({ editorPage }) => {
  await editorPage.mockUserDefinedFunctionInfo('final', 'func1')
  const initialNodesCount = await editorPage.nodeCount()
  await editorPage
    .selectNodes(['prod', 'sum', 'ten', 'twenty'])
    .clickActionTrigger('components.collapse')
    .expectNodeCount(initialNodesCount - 3)
    .mockUserDefinedFunctionInfo('prod', 'user_defined_component')
    .mockSuggestion({
      type: 'method',
      module: 'local.Mock_Project',
      name: 'user_defined_component',
      isStatic: true,
      arguments: [{ name: 'five', reprType: 'Any', isSuspended: false, hasDefault: false }],
      selfType: 'local.Mock_Project',
      returnType: 'Standard.Base.Any.Any',
      annotations: [],
    })
    .withNode('prod', async (node) => {
      const port = node.locator('.WidgetApplication.prefix > .WidgetPort')
      await expect(port).toExist()
      await expect(port).toHaveText('Main.user_defined_component')
      await expect(node.locator('.WidgetTopLevelArgument')).toHaveText('five')
    })
    .enterNode('prod')
    .expectNodeCount(6)
    .expectNodeCount(1, '.inputNode')
    .expectNodesToExist(['ten', 'sum', 'prod'])
    .selectNodes(['ten', 'sum'])
    .expectNodeCount(2, '.selected')
    .press(COLLAPSE_SHORTCUT)
    .expectNodeCount(5)
    .expectNodeCount(1, '.inputNode')
    .expectNodeTokens('sum', ['Main', '.', 'user_defined_component1', 'five', 'twenty'])
    .mockUserDefinedFunctionInfo('sum', 'user_defined_component1')
    .enterNode('sum')
    .expectNodesToExist(['ten'])
    .expectNodeCount(5)
})

test('Display message when User Defined Component ceases to exist', async ({ editorPage }) => {
  const initialNodesCount = await editorPage.nodeCount()
  await editorPage
    .selectNodes(['prod', 'sum'])
    .clickActionTrigger('components.collapse')
    .expectNodeCount(initialNodesCount - 1)
    .mockUserDefinedFunctionInfo('prod', 'user_defined_component')
    .enterNode('prod')
    .expectNodeCount(2)
    .press(`Mod+Z`)
    .do((page) => expect(page.locator('.GraphMissingView')).toExist())
})

test('Input node', async ({ editorPage }) => {
  await editorPage
    .call(enterToFunc2)
    .expectNodeCount(1, '.inputNode')
    .withNode('$INPUT', async (inputNode, page) => {
      // Input node with identifier should have the icon and an identifier.
      await expect(inputNode.locator('.WidgetIcon')).toHaveCount(1)
      await expect(inputNode.locator('.WidgetToken')).toContainText('a')
      const outputPort = await locate.outputPortCoordinates(page, inputNode)
      await page.mouse.click(outputPort.x + 20, outputPort.y)
      await locate.graphEditor(page).click({ position: { x: 100, y: 500 } })
      await expect(locate.componentBrowserInput(page)).toBeFocused()
      await page.keyboard.press('Escape')

      // Input node cannot be deleted
      await inputNode.locator('.grab-handle').click()
      await page.keyboard.press('Delete')
      await expect(inputNode).toHaveCount(1)
      await inputNode.locator('.More').click({})
      await expect(inputNode.getByTestId('action:components.deleteSelected')).toHaveClass(
        /(?<=^| )disabled(?=$| )/,
      )
    })

  // Input node has output port
})

test('Output node', async ({ editorPage }) => {
  await editorPage
    .call(enterToFunc2)
    .expectNodeCount(1, '.outputNode')

    .withNode('$OUTPUT', async (outputNode, page) => {
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
})

test('Output node is not collapsed', async ({ editorPage }) => {
  await editorPage
    .call(enterToFunc2)
    .selectNodes(['$OUTPUT', 'r'])
    .clickActionTrigger('components.collapse')
    .expectNodeTokens('r', ['Main', '.', 'user_defined_component', 'a'])
    .expectNodeCount(1)
})

test('Input node is not collapsed', async ({ editorPage }) => {
  await editorPage
    .call(enterToFunc2)
    .selectNodes(['r', '$INPUT'])
    .clickActionTrigger('components.collapse')
    .expectNodeTokens('r', ['Main', '.', 'user_defined_component', 'a'])
    .expectNodeCount(1, '.outputNode')
})

test('User Defined Component call shows argument placeholders', async ({ editorPage }) => {
  await editorPage
    .mockUserDefinedFunctionInfo('final', 'func1', [0])
    .mockSuggestion({
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
    .selectSingleNode('prod')
    .press(DELETE_KEY)
    .expectNodeCount(0, '.selected')
    .withNode('final', async (collapsedCallComponent, page) => {
      await expect(await edgesToNode(page, collapsedCallComponent)).toHaveCount(0)
      await expect(collapsedCallComponent.locator('.WidgetArgumentName .name')).toHaveText('arg1')
    })
})

function expectInsideMain(editorPage: EditorPageActions) {
  return editorPage
    .expectNodeCount(MAIN_FILE_NODES)
    .expectNodesToExist([
      'five',
      'ten',
      'sum',
      'prod',
      'final',
      'list',
      'data',
      'aggregated',
      'filtered',
      'autoscoped',
    ])
}

function expectInsideFunc1(editorPage: EditorPageActions) {
  return editorPage
    .expectNodeCount(4)
    .expectNodeCount(1, '.inputNode')
    .expectNodeCount(1, '.outputNode')
    .expectNodesToExist(['f2', 'result'])
    .do(async (page) => {
      // The mouse is often in output port area, making our checks fooled by the edge ghost.
      await page.mouse.move(0, 0)
      await expect(await edgesFromNode(page, locate.inputNode(page))).toHaveCount(EDGE_PARTS)
      await expect(await edgesToNode(page, locate.outputNode(page))).toHaveCount(EDGE_PARTS)
    })
}

function expectInsideFunc2(editorPage: EditorPageActions) {
  return editorPage
    .expectNodeCount(3)
    .expectNodeCount(1, '.inputNode')
    .expectNodeCount(1, '.outputNode')
    .expectNodesToExist(['r'])
    .do(async (page) => {
      // The mouse is often in output port area, making our checks fooled by the edge ghost.
      await page.mouse.move(0, 0)
      await expect(await edgesFromNode(page, locate.inputNode(page))).toHaveCount(EDGE_PARTS)
      await expect(await edgesToNode(page, locate.outputNode(page))).toHaveCount(EDGE_PARTS)
    })
}

function enterToFunc2(editorPage: EditorPageActions) {
  return editorPage
    .mockUserDefinedFunctionInfo('final', 'func1')
    .enterNode('final')
    .call(expectInsideFunc1)
    .mockUserDefinedFunctionInfo('f2', 'func2')
    .enterNode('f2')
    .call(expectInsideFunc2)
}
