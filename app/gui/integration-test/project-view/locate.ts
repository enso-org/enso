import { expect, type Locator, type Page } from 'integration-test/base'
import assert from 'node:assert'

// ================
// === Locators ===
// ================

// === Button locators ===

function or(a: (page: Locator | Page) => Locator, b: (page: Locator | Page) => Locator) {
  return (page: Locator | Page) => a(page).or(b(page))
}

/** Show/hide visualization button */
export function toggleVisualizationButton(page: Locator | Page) {
  return page.getByLabel(/(Show|Hide|Show\/Hide) visualization.*/).first()
}

/** Visualization Selector button */
export function toggleVisualizationSelectorButton(page: Locator | Page) {
  return page.getByLabel('Visualization Selector')
}

// === Fullscreen ===

/** Enter fullscreen */
export function enterFullscreenButton(page: Locator | Page) {
  return page.getByLabel('Fullscreen')
}

/** Exit fullscreen */
export function exitFullscreenButton(page: Locator | Page) {
  return page.getByLabel('Exit Fullscreen')
}

export const toggleFullscreenButton = or(enterFullscreenButton, exitFullscreenButton)

// === Nodes ===

declare const nodeLocatorBrand: unique symbol

/** A locator which resolves to graph nodes only */
export type Node = Locator & { [nodeLocatorBrand]: never }

/** Filter selector that only matches input nodes. */
export const INPUT_NODE_FILTER = '.inputNode'
/** Filter selector that only matches output nodes. */
export const OUTPUT_NODE_FILTER = '.outputNode'

/** All nodes in graph */
export function graphNode(page: Page | Locator): Node {
  return page.locator('.GraphNode') as Node
}
/** Node with given binding (name) */
export function graphNodeByBinding(page: Locator | Page, binding: string): Node {
  return graphNode(page).filter({
    has: page.locator('.binding').getByText(binding, { exact: true }),
  }) as Node
}
/** Icon inside the node */
export function graphNodeIcon(node: Node) {
  return node.locator('.nodeCategoryIcon')
}
/** All selected nodes */
export function selectedNodes(page: Page | Locator): Node {
  return page.locator('.GraphNode.selected') as Node
}
/** All input nodes */
export function inputNode(page: Page | Locator): Node {
  return page.locator('.GraphNode.inputNode') as Node
}
/** All output nodes  */
export function outputNode(page: Page | Locator): Node {
  return page.locator('.GraphNode.outputNode') as Node
}

// === Data locators ===

function componentLocator(locatorStr: string) {
  return (page: Locator | Page) => page.locator(locatorStr)
}

function testIdLocator(testId: string) {
  return (page: Locator | Page) => page.getByTestId(testId)
}

export const graphEditor = componentLocator('.GraphEditor')
export const codeEditor = componentLocator('.CodeEditor')
export const anyVisualization = componentLocator('.GraphVisualization')
export const loadingVisualization = componentLocator('.LoadingVisualization')
export const componentMenu = componentLocator('.ComponentMenu')
export const componentMenuMoreEntries = testIdLocator('component-menu-more-entries')
export const addNewNodeButton = testIdLocator('add-component-button')
export const componentBrowser = componentLocator('.ComponentBrowser')
export const componentBrowserInput = testIdLocator('component-editor-content')
export const nodeOutputPort = componentLocator('.outputPortHoverArea')
export const nodeComment = componentLocator('.GraphNodeComment')
export const nodeCommentContent = testIdLocator('graph-node-comment-content')

/**
 * A not-selected variant of Component Browser Entry.
 *
 * It may be covered by selected one due to way we display them.
 */
export function componentBrowserEntry(page: Locator | Page) {
  return page.locator(`.ComponentEntry`)
}

/** A selected variant of Component Browser Entry */
export function componentBrowserSelectedEntry(page: Locator | Page) {
  return page.locator(`.ComponentEntry.selected`)
}

/** A not-selected variant of Component Browser entry with given label */
export function componentBrowserEntryByLabel(page: Locator | Page, label: string) {
  return componentBrowserEntry(page).filter({ has: page.getByText(label) })
}

/** Right-docked panel */
export function rightDock(page: Page) {
  return page.getByTestId('right-panel')
}

/** Bottom-docked panel */
export function bottomDock(page: Page) {
  return page.getByTestId('bottomDock')
}

/** Button to add an item to a vector */
export function addItemButton(page: Locator | Page) {
  return page.getByRole('button', { name: 'new item' })
}

/** Button to delete a specific item from a vector */
export function deleteItemButton(page: Locator | Page) {
  return page.getByRole('button', { name: 'Remove item' })
}

export const navBreadcrumb = componentLocator('.NavBreadcrumb')

function visualizationLocator(visSelector: string) {
  // Playwright pierces shadow roots, but not within a single XPath.
  // Locate the visualization content, then locate the descendant.
  const visLocator = componentLocator(visSelector)
  return (page: Locator | Page) => {
    const hostLocator = page.locator('.VisualizationHostContainer')
    return visLocator(hostLocator)
  }
}

export const jsonVisualization = visualizationLocator('.JSONVisualization')
export const tableVisualization = visualizationLocator('.TableVisualization')
export const scatterplotVisualization = visualizationLocator('.ScatterplotVisualization')
export const histogramVisualization = visualizationLocator('.HistogramVisualization')
export const heatmapVisualization = visualizationLocator('.HeatmapVisualization')
export const sqlVisualization = visualizationLocator('.SqlVisualization')
export const geoMapVisualization = visualizationLocator('.GeoMapVisualization')
export const imageBase64Visualization = visualizationLocator('.ImageBase64Visualization')
export const warningsVisualization = visualizationLocator('.WarningsVisualization')

/** Type label on the visualisation */
export function visualisationNodeType(page: Page) {
  return page.getByTestId('visualisationNodeType')
}

// === Edge locators ===

/** All edges going from a node with given binding that are connected to another node. */
export async function connectedEdgesFromNodeWithBinding(page: Page, binding: string) {
  return edgesFromNode(page, graphNodeByBinding(page, binding).first())
}

/** All edges going from a node. */
export async function edgesFromNode(page: Page, node: Locator) {
  const nodeId = await node.getAttribute('data-node-id')
  return page.locator(`[data-source-node-id="${nodeId}"]`)
}

/** All edges going to a node with given binding. */
export async function edgesToNodeWithBinding(page: Page, binding: string) {
  return edgesToNode(page, graphNodeByBinding(page, binding).first())
}

/** All edges going to a node. */
export async function edgesToNode(page: Page, node: Locator) {
  const nodeId = await node.getAttribute('data-node-id')
  return page.locator(`[data-target-node-id="${nodeId}"]`)
}

// === Output ports ===

/**
 * Returns a location that can be clicked to activate an output port.
 * Using a `Locator` would be better, but `position` option of `click` doesn't work.
 */
export async function outputPortCoordinates(page: Page, node: Locator) {
  const nodeId = await node.getAttribute('data-node-id')
  const outputPortArea = await page
    .locator(`.GraphNodeOutputPorts[data-output-ports-node-id="${nodeId}"] .outputPortHoverArea`)
    .boundingBox()
  await expect(outputPortArea).toBeTruthy()
  assert(outputPortArea)
  const centerX = outputPortArea.x + outputPortArea.width / 2
  const bottom = outputPortArea.y + outputPortArea.height
  return { x: centerX, y: bottom - 2.0 }
}

/** Returns a locator for the create node from port button. */
export async function createNodeFromPortButton(page: Page, node: Locator) {
  const nodeId = await node.getAttribute('data-node-id')
  const button = page.locator(
    `.GraphNodeOutputPorts[data-output-ports-node-id="${nodeId}"] .CreateNodeFromPortButton`,
  )
  // Ensure the animation is complete.
  await button.elementHandle().then((el) => el!.waitForElementState('stable'))
  return button
}
