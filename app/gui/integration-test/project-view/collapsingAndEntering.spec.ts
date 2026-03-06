import type EditorPageActions from 'integration-test/actions/EditorPageActions'
import type { RelativePos } from 'integration-test/actions/EditorPageActions'
import { expect, test } from 'integration-test/base'
import { DELETE_KEY } from './keyboard'
import * as locate from './locate'

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
            'user_defined_component',
          )
        })
        .enterNode('prod')
        .expectNodeCount(6)
        .expectNodeCount(3, locate.INPUT_NODE_FILTER)
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
      await expect(port).toHaveText('user_defined_component')
      await expect(node.locator('.WidgetTopLevelArgument')).toHaveText('five')
    })
    .enterNode('prod')
    .expectNodeCount(6)
    .expectNodeCount(1, locate.INPUT_NODE_FILTER)
    .expectNodesToExist(['ten', 'sum', 'prod'])
    .selectNodes(['ten', 'sum'])
    .expectNodeCount(2, '.selected')
    .press(COLLAPSE_SHORTCUT)
    .expectNodeCount(5)
    .expectNodeCount(1, locate.INPUT_NODE_FILTER)
    .do((page) => page.keyboard.type('My renamed component'))
    .press('Enter')
    .expectNodeTokens('sum', ['my_renamed_component'])
    .expectArgumentPlaceholders('sum', ['five', 'twenty'])
    .mockUserDefinedFunctionInfo('sum', 'my_renamed_component')
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
    .expectNodeCount(6)
    .press(`Mod+Z`)
    .do((page) => expect(page.locator('.GraphMissingView')).toExist())
})

test('Input node', async ({ editorPage }) => {
  await editorPage
    .call(enterToFunc2)
    .expectNodeCount(1, locate.INPUT_NODE_FILTER)
    .withNode(locate.INPUT_NODE_FILTER, async (inputNode, page) => {
      // Input node with identifier should have the icon and an identifier.
      await expect(inputNode.locator('.WidgetIcon')).toHaveCount(1)
      await expect(inputNode.locator('.WidgetToken')).toContainText('a')

      // Input node has output port
      const outputPort = await locate.outputPortCoordinates(page, inputNode)
      await page.mouse.click(outputPort.x + 20, outputPort.y)
      await locate.graphEditor(page).click({ position: { x: 100, y: 500 } })
      await expect(locate.componentBrowserInput(page)).toBeFocused()
    })
    .press('Escape')
    .clickActionTrigger('graph.fitAll')
    // Input node cannot be deleted
    .selectSingleNode(locate.INPUT_NODE_FILTER)
    .press(DELETE_KEY)
    .expectNodeCount(1, locate.INPUT_NODE_FILTER)
    .withNode(locate.INPUT_NODE_FILTER, async (node) => {
      await node.locator('.More').click()
      await expect(node.getByTestId('action:components.deleteSelected')).toHaveClass(
        /(?<=^| )disabled(?=$| )/,
      )
    })
})

test('Output node', async ({ editorPage }) => {
  await editorPage
    .call(enterToFunc2)
    .expectNodeCount(1, locate.OUTPUT_NODE_FILTER)

    .withNode(locate.OUTPUT_NODE_FILTER, async (outputNode) => {
      // Output node with identifier should have only icon and no displayed identifiers
      await expect(outputNode.locator('.WidgetIcon')).toHaveCount(1)
      await expect(outputNode.locator('.WidgetToken')).toHaveCount(0)
    })
    // Output node cannot be deleted
    .selectSingleNode(locate.OUTPUT_NODE_FILTER)
    .press(DELETE_KEY)
    .expectNodeCount(1, locate.OUTPUT_NODE_FILTER)
    .withNode(locate.OUTPUT_NODE_FILTER, async (node) => {
      await node.locator('.More').click()
      await expect(node.getByTestId('action:components.deleteSelected')).toHaveClass(
        /(?<=^| )disabled(?=$| )/,
      )
    })
})

test('Output node is not collapsed', async ({ editorPage }) => {
  await editorPage
    .call(enterToFunc2)
    .selectNodes([locate.OUTPUT_NODE_FILTER, 'r'])
    .clickActionTrigger('components.collapse')
    .press('Enter')
    .expectNodeTokens('r', ['user_defined_component'])
    .expectArgumentPlaceholders('r', ['a'])
    .expectNodeCount(3)
})

test('Input node is not collapsed', async ({ editorPage }) => {
  await editorPage
    .call(enterToFunc2)
    .selectNodes(['r', locate.INPUT_NODE_FILTER])
    .clickActionTrigger('components.collapse')
    .press('Enter')
    .expectNodeTokens('r', ['user_defined_component'])
    .expectArgumentPlaceholders('r', ['a'])
    .expectNodeCount(3)
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
    .expectEdgesFromTo(undefined, 'final', 0)
    .withNode('final', async (node) =>
      expect(node.locator('.WidgetArgumentName .name')).toHaveText('arg1'),
    )
})

function expectInsideMain(editorPage: EditorPageActions) {
  return editorPage
    .expectNodeCount(14)
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
    .expectNodeCount(1, locate.INPUT_NODE_FILTER)
    .expectNodeCount(1, locate.OUTPUT_NODE_FILTER)
    .expectNodesToExist(['f2', 'result'])
    .expectEdgesFromTo(locate.INPUT_NODE_FILTER, undefined)
    .expectEdgesFromTo(undefined, locate.OUTPUT_NODE_FILTER)
}

function expectInsideFunc2(editorPage: EditorPageActions) {
  return editorPage
    .expectNodeCount(3)
    .expectNodeCount(1, locate.INPUT_NODE_FILTER)
    .expectNodeCount(1, locate.OUTPUT_NODE_FILTER)
    .expectNodesToExist(['r'])
    .expectEdgesFromTo(locate.INPUT_NODE_FILTER, undefined)
    .expectEdgesFromTo(undefined, locate.OUTPUT_NODE_FILTER)
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
