import { expect, type Locator } from 'playwright/test'
import type { NodeLocator, WidgetLocator } from './locate'

/** Ensures that at least one of the elements that the Locator points to,
 * is an attached and visible DOM node. */
export function toExist(locator: Locator) {
  // Counter-intuitive, but correct:
  // https://playwright.dev/docs/api/class-locatorassertions#locator-assertions-to-be-visible
  return expect(locator.first()).toBeVisible()
}

export function toHaveTokens(locator: NodeLocator | WidgetLocator, tokens: string[]) {
  return expect(locator.locator('.WidgetToken')).toHaveText(tokens)
}
