import { expect, type Locator, type Page } from '@playwright/test'
import assert from 'assert'

// ================
// === Locators ===
// ================

// === Button locators ===

function or(a: (page: Locator | Page) => Locator, b: (page: Locator | Page) => Locator) {
  return (page: Locator | Page) => a(page).or(b(page))
}

/** Show/hide visualization button */
export function toggleVisualizationButton(page: Locator | Page) {
  return page.getByLabel('Visualization', { exact: true })
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

/** All nodes in graph */
export function graphNode(page: Page | Locator): Node {
  return page.locator('.GraphNode') as Node
}
/** Node with given binding (name) */
export function graphNodeByBinding(page: Locator | Page, binding: string): Node {
  return graphNode(page).filter({ has: page.locator('.binding', { hasText: binding }) }) as Node
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
  return (page: Locator | Page) => {
    return page.locator(`${locatorStr}`)
  }
}

export const graphEditor = componentLocator('.GraphEditor')
export const codeEditor = componentLocator('.CodeEditor')
export const anyVisualization = componentLocator('.GraphVisualization')
export const loadingVisualization = componentLocator('.LoadingVisualization')
export const circularMenu = componentLocator('.CircularMenu')
export const addNewNodeButton = componentLocator('.PlusButton')
export const componentBrowser = componentLocator('.ComponentBrowser')
export const nodeOutputPort = componentLocator('.outputPortHoverArea')
export const smallPlusButton = componentLocator('.SmallPlusButton')
export const lexicalContent = componentLocator('.LexicalContent')

/**
 * A not-selected variant of Component Browser Entry.
 *
 * It may be covered by selected one due to way we display them.
 */
export function componentBrowserEntry(page: Locator | Page) {
  return page.locator(`.ComponentBrowser .list-variant:not(.selected) .component`)
}

/** A selected variant of Component Browser Entry */
export function componentBrowserSelectedEntry(page: Locator | Page) {
  return page.locator(`.ComponentBrowser .list-variant.selected .component`)
}

/** A not-selected variant of Component Browser entry with given label */
export function componentBrowserEntryByLabel(page: Locator | Page, label: string) {
  return componentBrowserEntry(page).filter({ has: page.getByText(label) })
}

/** Right-docked panel */
export function rightDock(page: Page) {
  return page.getByTestId('rightDock')
}

/** rightDock, but also includes toggle button */
export function rightDockRoot(page: Page) {
  return page.getByTestId('rightDockRoot')
}

/** Bottom-docked panel */
export function bottomDock(page: Page) {
  return page.getByTestId('bottomDock')
}

export const navBreadcrumb = componentLocator('.NavBreadcrumb')
export const componentBrowserInput = componentLocator('.ComponentEditor')

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

// === Edge locators ===

/** All edges going from a node with given binding. */
export async function edgesFromNodeWithBinding(page: Page, binding: string) {
  const node = graphNodeByBinding(page, binding).first()
  const nodeId = await node.getAttribute('data-node-id')
  return page.locator(`[data-source-node-id="${nodeId}"]`)
}

/** All edges going to a node with given binding. */
export async function edgesToNodeWithBinding(page: Page, binding: string) {
  const node = graphNodeByBinding(page, binding).first()
  const nodeId = await node.getAttribute('data-node-id')
  return page.locator(`[data-target-node-id="${nodeId}"]`)
}

// === Output ports ===

/**
 * Returns a location that can be clicked to activate an output port.
 * Using a `Locator` would be better, but `position` option of `click` doesn't work.
 */
export async function outputPortCoordinates(node: Locator) {
  const outputPortArea = await node.locator('.outputPortHoverArea').boundingBox()
  expect(outputPortArea).not.toBeNull()
  assert(outputPortArea)
  const centerX = outputPortArea.x + outputPortArea.width / 2
  const bottom = outputPortArea.y + outputPortArea.height
  return { x: centerX, y: bottom - 2.0 }
}
