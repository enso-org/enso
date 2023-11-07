import { expect, type Locator, type Page } from '@playwright/test'
import cssEscape from 'css.escape'

// ==============
// === Filter ===
// ==============

export class Filter {
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

// =================
// === goToGraph ===
// =================

/** Perform a successful login. */
export async function goToGraph(page: Page) {
  await page.goto('/')
  // Originally this clicked the play button but for now that is out of scope.
}

// ==================
// === Assertions ===
// ==================

export async function expectToExist(locator: Locator) {
  expect(await locator.count()).toBeGreaterThan(0)
}

// ================
// === Locators ===
// ================

// === Button locators ===

/** Find a button to open the editor (if any) on the current page. */
export function locatePlayOrOpenProjectButton(page: Locator | Page) {
  return page.getByAltText('Open in editor')
}

// === Data locators ===

type SanitizeClassName<T extends string> = T extends `${infer A}.${infer B}`
  ? SanitizeClassName<`${A}${B}`>
  : T extends `${infer A} ${infer B}`
  ? SanitizeClassName<`${A}${B}`>
  : T

function componentLocator<T extends string>(className: SanitizeClassName<T>) {
  return (page: Locator | Page, filter?: (f: Filter) => { selector: string }) => {
    return page.locator(`.${className}${filter?.(new Filter()) ?? ''}`)
  }
}

export const locateGraphEditor = componentLocator('GraphEditor')
export const locateGraphNode = componentLocator('GraphNode')
export const locateGraphVisualizations = componentLocator('GraphVisualization')
