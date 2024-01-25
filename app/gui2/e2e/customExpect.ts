import { expect, type Locator } from 'playwright/test'

/** Ensures that at least one of the elements that the Locator points to,
 * is an attached and visible DOM node. */
export function toExist(locator: Locator) {
  // Counter-intuitive, but correct:
  // https://playwright.dev/docs/api/class-locatorassertions#locator-assertions-to-be-visible
  return expect(locator.first()).toBeVisible()
}
