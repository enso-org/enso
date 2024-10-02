import { expect, type Locator, type Page } from '@playwright/test'
import assert from 'assert'
import cssEscape from 'css.escape'

// ==============
// === Filter ===
// ==============

class Filter {
  constructor(public selector = '') {}

  visible<T extends { selector: string }>(this: T): Omit<T, 'visible'> {
    return new Filter(this.selector + ':visible') as any
  }

  first<T extends { selector: string }>(this: T): Omit<T, 'first' | 'last'> {
    return new Filter(this.selector + ':first') as any
  }

  last<T extends { selector: string }>(this: T): Omit<T, 'first' | 'last'> {
    return new Filter(this.selector + ':last') as any
  }

  id<T extends { selector: string }>(this: T, id: string): Omit<T, 'id'> {
    return new Filter(this.selector + '#' + cssEscape(id)) as any
  }

  class(...classes: string[]) {
    return new Filter(this.selector + '.' + classes.map(cssEscape).join('.'))
  }

  toString() {
    return this.selector
  }
}

// ================
// === Locators ===
// ================

// === Button locators ===

function or(a: (page: Locator | Page) => Locator, b: (page: Locator | Page) => Locator) {
  return (page: Locator | Page) => a(page).or(b(page))
}

/** TODO: Add docs */
export function toggleVisualizationButton(page: Locator | Page) {
  return page.getByLabel('Visualization', { exact: true })
}

/** TODO: Add docs */
export function toggleVisualizationSelectorButton(page: Locator | Page) {
  return page.getByLabel('Visualization Selector')
}

// === Fullscreen ===

/** TODO: Add docs */
export function enterFullscreenButton(page: Locator | Page) {
  return page.getByLabel('Fullscreen')
}

/** TODO: Add docs */
export function exitFullscreenButton(page: Locator | Page) {
  return page.getByLabel('Exit Fullscreen')
}

export const toggleFullscreenButton = or(enterFullscreenButton, exitFullscreenButton)

// === Nodes ===

declare const nodeLocatorBrand: unique symbol
export type Node = Locator & { [nodeLocatorBrand]: never }

/** TODO: Add docs */
export function graphNode(page: Page | Locator): Node {
  return page.locator('.GraphNode') as Node
}
/** TODO: Add docs */
export function graphNodeByBinding(page: Locator | Page, binding: string): Node {
  return graphNode(page).filter({ has: page.locator('.binding', { hasText: binding }) }) as Node
}
/** TODO: Add docs */
export function graphNodeIcon(node: Node) {
  return node.locator('.nodeCategoryIcon')
}
/** TODO: Add docs */
export function selectedNodes(page: Page | Locator): Node {
  return page.locator('.GraphNode.selected') as Node
}
/** TODO: Add docs */
export function inputNode(page: Page | Locator): Node {
  return page.locator('.GraphNode.inputNode') as Node
}
/** TODO: Add docs */
export function outputNode(page: Page | Locator): Node {
  return page.locator('.GraphNode.outputNode') as Node
}

// === Data locators ===

function componentLocator(locatorStr: string) {
  return (page: Locator | Page, filter?: (f: Filter) => { selector: string }) => {
    return page.locator(`${locatorStr}${filter?.(new Filter()) ?? ''}`)
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

/** TODO: Add docs */
export function componentBrowserEntry(
  page: Locator | Page,
  filter?: (f: Filter) => { selector: string },
) {
  return page.locator(
    `.ComponentBrowser .list-variant:not(.selected) .component${filter?.(new Filter()) ?? ''}`,
  )
}

/** TODO: Add docs */
export function componentBrowserSelectedEntry(
  page: Locator | Page,
  filter?: (f: Filter) => { selector: string },
) {
  return page.locator(
    `.ComponentBrowser .list-variant.selected .component${filter?.(new Filter()) ?? ''}`,
  )
}

/** TODO: Add docs */
export function componentBrowserEntryByLabel(page: Locator | Page, label: string) {
  return componentBrowserEntry(page).filter({ has: page.getByText(label) })
}

/** TODO: Add docs */
export function rightDock(page: Page) {
  return page.getByTestId('rightDock')
}

/** rightDock, but also includes toggle button */
export function rightDockRoot(page: Page) {
  return page.getByTestId('rightDockRoot')
}

/** TODO: Add docs */
export function bottomDock(page: Page) {
  return page.getByTestId('bottomDock')
}

export const navBreadcrumb = componentLocator('.NavBreadcrumb')
export const componentBrowserInput = componentLocator('.ComponentEditor')

function visualizationLocator(visSelector: string) {
  // Playwright pierces shadow roots, but not within a single XPath.
  // Locate the visualization content, then locate the descendant.
  const visLocator = componentLocator(visSelector)
  return (page: Locator | Page, filter?: (f: Filter) => { selector: string }) => {
    const hostLocator = page.locator('.VisualizationHostContainer')
    return visLocator(hostLocator, filter)
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

/** TODO: Add docs */
export async function edgesFromNodeWithBinding(page: Page, binding: string) {
  const node = graphNodeByBinding(page, binding).first()
  const nodeId = await node.getAttribute('data-node-id')
  return page.locator(`[data-source-node-id="${nodeId}"]`)
}

/** TODO: Add docs */
export async function edgesToNodeWithBinding(page: Page, binding: string) {
  const node = graphNodeByBinding(page, binding).first()
  const nodeId = await node.getAttribute('data-node-id')
  return page.locator(`[data-target-node-id="${nodeId}"]`)
}

// === Output ports ===

/**
 * Returns a location that can be clicked to activate an output port.
 *  Using a `Locator` would be better, but `position` option of `click` doesn't work.
 */
export async function outputPortCoordinates(node: Locator) {
  const outputPortArea = await node.locator('.outputPortHoverArea').boundingBox()
  expect(outputPortArea).not.toBeNull()
  assert(outputPortArea)
  const centerX = outputPortArea.x + outputPortArea.width / 2
  const bottom = outputPortArea.y + outputPortArea.height
  return { x: centerX, y: bottom - 2.0 }
}
