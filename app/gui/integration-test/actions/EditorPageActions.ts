/** @file Actions for the "editor" page. */
import { expect, type Locator, type Page } from 'integration-test/base'
import type { ExpressionLocator } from 'integration-test/project-view/expressionUpdates'
import type { ExpressionUpdate, MethodCall } from 'ydoc-shared/languageServerTypes'
import type { SuggestionEntry } from 'ydoc-shared/languageServerTypes/suggestions'
import * as locate from '../project-view/locate'
import { goToPageActions, type GoToPageActions } from './goToPageActions'
import PageActions from './PageActions'

export interface RelativePos {
  relativeTo?: string
  x: number
  y: number
}

function printRelativePos(pos: RelativePos) {
  const relative = pos.relativeTo ? `${pos.relativeTo}+` : ''
  return `${relative}(${pos.x},${pos.y})`
}

/** Actions for the "editor" page. */
export default class EditorPageActions<Context = object> extends PageActions<Context> {
  /** Actions for navigating to another page. */
  get goToPage(): Omit<GoToPageActions<Context>, 'projectView'> {
    return goToPageActions(this.step.bind(this))
  }

  /** Wait for the editor to load. */
  waitForEditorToLoad(): EditorPageActions<Context> {
    return this.step('Wait for the project view to load', async () => {
      await this.page.waitForSelector('.ProjectView', { state: 'visible' })
    })
  }

  /** Close all toast notifications. */
  closeToastNotifications() {
    return this.step('Close toast notifications', async () => {
      await Promise.all(
        await this.page
          .locator('.Toastify__toast')
          .getByRole('button')
          .all()
          .then((buttons) => buttons.map((button) => button.click())),
      )
    })
  }

  /** Wait for node locations to be initialized. */
  expectNodePositionsInitialized() {
    return this.step('Expect node positions initialized', async (page) => {
      // Wait until edges are initialized and displayed correctly.
      await expect(page.getByTestId('broken-edge')).toBeHidden()
      // Wait until node sizes are initialized.
      await expect(locate.graphNode(this.page).first().locator('.nodeBackground')).toBeVisible()
      await expect(locate.graphNode(this.page).first()).toHaveCSS(
        'transform',
        // TODO: The transform matrix should not need to be a regex. Instead, first automatically positioned nodes
        // should always have constant known position. This is a bug caused by incorrect layout after
        // entering a function. To be fixed with #9255
        // Expected constant: `matrix(1, 0, 0, 1, -16, -16)`
        /matrix\(1, 0, 0, 1, -16, -?\d+\)/,
      )
    })
  }

  /** Locate a graph node and perform action on it. */
  withNode(binding: string, callback: (node: Locator, page: Page) => void | Promise<void>) {
    return this.do(async () => callback(await this.locateNodes(binding), this.page))
  }

  /** Locate a graph node with given binding or class filter. Returns all present nodes if nothing provided. */
  locateNodes(bindingOrFilter?: string) {
    if (!bindingOrFilter) {
      return locate.graphNode(this.page)
    } else if (!treatAsBinding(bindingOrFilter)) {
      return locate.graphNode(this.page).and(this.page.locator(bindingOrFilter))
    } else {
      return locate.graphNodeByBinding(this.page, bindingOrFilter)
    }

    function treatAsBinding(bindingOrFilter: string) {
      // Decide whether or not the argument should be interpreted as a binding or a filter.
      // Assume anything that looks like simple variable name is a binding, otherwise assume it is a filter.
      // Common filters start with dot (class name).
      return /^[a-z0-9_]+$/.test(bindingOrFilter)
    }
  }

  /**
   * Assert for specific number of top-level method arguments present on a node.
   * Only counts currently visible arguments, which might be different if node is expanded or collapsed.
   */
  expectNodeTopLevelArgumentCount(bindingOrFilter: string, count: number) {
    return this.withNode(bindingOrFilter, async (node) => {
      await expect(node.locator('.WidgetTopLevelArgument')).toHaveCount(count)
    })
  }

  /** Check for existence or absence of edges that match given source/target specification. */
  expectEdgesFromTo(
    sourceNode: string | undefined,
    targetNode: string | undefined,
    expectedCount = 1,
  ) {
    return this.step(`Expect edge '${sourceNode || '*'}' -> '${targetNode || '*'}'`, async () => {
      // The mouse is often in output port area, making our checks fooled by the edge ghost.
      await this.page.mouse.move(0, 0)

      const [sourceId, targetId] = await Promise.all([
        sourceNode ? this.locateNodes(sourceNode).getAttribute('data-node-id') : undefined,
        targetNode ? this.locateNodes(targetNode).getAttribute('data-node-id') : undefined,
      ])
      if (sourceNode) expect(sourceId).toBeDefined()
      if (targetNode) expect(targetId).toBeDefined()
      let edgeLocator = 'g.GraphEdge'
      if (sourceId) edgeLocator += `[data-source-node-id="${sourceId}"]`
      if (targetId) edgeLocator += `[data-target-node-id="${targetId}"]`
      await expect(this.page.locator(edgeLocator)).toHaveCount(expectedCount, { timeout: 1000 })
    })
  }

  /** Check if current graph breadcrumbs match expected value. */
  expectBreadcrumbs(expectedBreadcrumbs: (string | RegExp)[], extraLocator?: string) {
    return this.step('Check breadcrumbs', async (page) => {
      const locator = locate.navBreadcrumb(page)
      const finalLocator = extraLocator ? locator.and(page.locator(extraLocator)) : locator

      await expect(finalLocator).toHaveText(expectedBreadcrumbs)
    })
  }

  /** Perform a click action on a breadcrumb part containing given text. */
  clickBreadcrumb(crumb: string | RegExp) {
    return this.step(`Click breadcrumb ${crumb}`, async (page) => {
      await locate.navBreadcrumb(page).filter({ hasText: crumb }).first().click()
    })
  }

  /** Get count of nodes in the graph. */
  nodeCount(filter?: string) {
    return this.locateNodes(filter).count()
  }

  /** Expect count of nodes to reachto given value. */
  expectNodeCount(expected: number, filter?: string) {
    return this.step(`Expect node count to be ${expected}`, () =>
      expect(this.locateNodes(filter)).toHaveCount(expected),
    )
  }

  /** Expect a node with given binding to contain specified list of tokens. */
  expectNodeTokens(binding: string, expectedTokens: string[]) {
    return this.withNode(binding, async (node) => {
      await expect(node.locator('.WidgetToken')).toHaveText(expectedTokens)
    })
  }

  /** Expect a node with given binding to contain specified list of tokens. */
  expectArgumentPlaceholders(binding: string, expectedPlaceholders: string[]) {
    return this.withNode(binding, async (node) => {
      await expect(node.locator('.WidgetArgumentName .name')).toHaveText(expectedPlaceholders)
    })
  }

  /** Expect count of nodes to reachto given value. */
  expectNodesToExist(nodeBindings: string[]) {
    return this.do(async () => {
      for (const binding of nodeBindings) {
        await expect(this.locateNodes(binding)).toExist()
      }
    })
  }

  /** Expect only and exactly nodes with specified bindings to be selected. */
  expectSelectedNodesExactly(nodeBindings: string[]) {
    return this.do(async () => {
      const bindings = await this.locateNodes('.selected').locator('.binding').all()
      const bindingTexts = await Promise.all(bindings.map((b) => b.textContent()))
      await expect(bindingTexts).toStrictEqual(nodeBindings)
    })
  }

  /** Expect an exact sequence of input nodes to exist within the graph, positioned in order from left to right. */
  expectInputNodesInOrder(expectedOrder: string[]) {
    return this.step('Expect input nodes in order', async () => {
      const inputNodes = await this.locateNodes(locate.INPUT_NODE_FILTER).all()
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
    })
  }

  /** Drag a node to specified location using its grab handle. */
  dragNode(nodeBinding: string, targetPosition: RelativePos) {
    return this.step(
      `Drag node '${nodeBinding}' to ${printRelativePos(targetPosition)}`,
      async () => {
        const node = this.locateNodes(nodeBinding)
        const grabHandle = node.locator('.grab-handle')
        const dragTarget =
          targetPosition.relativeTo ?
            this.locateNodes(targetPosition.relativeTo).locator('.grab-handle')
          : grabHandle
        await grabHandle.dragTo(dragTarget, { targetPosition, force: true })
      },
    )
  }

  /** Reset node selection by clicking on the graph background. */
  clearSelection() {
    return this.step('Clear selection', async (page) => {
      // TODO: Automatically find graph background region that is safe to click,
      // or somehow ensure that the dispatched click event performs
      // Current value chosen just so it worked for existing tests.
      await page.mouse.click(250, 300)
      await this.expectNodeCount(0, '.selected')
    })
  }

  /** Select a node by clicking on it without modifiers. */
  selectSingleNode(binding: string) {
    return this.step(`Select node '${binding}'`, async () => {
      await this.locateNodes(binding).locator('.grab-handle').click()
      await this.expectNodeCount(1, '.selected')
    })
  }

  /** Add given nodes to selection. */
  selectNodes(nodeBindings: string[]) {
    return this.step(`Select ${nodeBindings.length} nodes`, async () => {
      for (const binding of nodeBindings) {
        await this.locateNodes(binding)
          .locator('.grab-handle')
          .click({ modifiers: ['Shift'] })
      }
    })
  }

  /** Locate a button or menu entry representing given action and click it. */
  clickActionTrigger(actionName: string, onlyContextMenu = false) {
    return this.step(`Click action ${actionName}`, async (page) => {
      let locator = page.getByTestId(`action:${actionName}`)
      if (onlyContextMenu) locator = locator.and(page.locator('.ContextMenuEntry'))
      await locator.click()
    })
  }

  /** Double-click on a node to enter it. */
  enterNode(binding: string) {
    return this.step(`Enter node '${binding}'`, async () => {
      await this.locateNodes(binding).locator('.grab-handle').dblclick()
    })
  }

  /** Exit the currently opened graph (of User Defined Component). */
  exitFunction(x = 300, y = 300) {
    return this.step(`Exit function`, async (page) => {
      await locate.graphEditor(page).dblclick({ position: { x, y } })
    })
  }

  /*********/
  /* MOCKS */
  /*********/

  /** Provide custom expression update for the specific node. */
  mockExpressionUpdate(expression: ExpressionLocator, update: Partial<ExpressionUpdate>) {
    return this.step('Mock expression update', (page) =>
      page.evaluate(
        // TODO: use mock API call to issue the expression update instead.
        ({ expression, update }) => (window as any)._mockExpressionUpdate(expression, update),
        { expression, update },
      ),
    )
  }

  /** Add an entry to the suggestion database. */
  mockSuggestion(update: SuggestionEntry) {
    return this.step('Mock suggestion', async (page) => {
      // TODO: use mock API call to issue the suggestion update instead.
      await page.evaluate(({ update }) => (window as any)._mockSuggestion(update), { update })
    })
  }

  /** Provide custom method call info for the specific node. */
  mockMethodCallInfo(expression: ExpressionLocator, methodCallInfo: MethodCall) {
    return this.mockExpressionUpdate(expression, { methodCall: methodCallInfo })
  }

  /** Provide method call info for User Defined Function call. */
  mockUserDefinedFunctionInfo(
    expression: ExpressionLocator,
    functionName: string,
    notAppliedArguments: number[] = [],
  ) {
    return this.mockMethodCallInfo(expression, {
      methodPointer: {
        module: 'local.Mock_Project.Main',
        definedOnType: 'local.Mock_Project.Main',
        name: functionName,
      },
      notAppliedArguments,
    })
  }
}
