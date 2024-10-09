import { expect as baseExpect, type Locator } from 'playwright/test'

export const expect = baseExpect.extend({
  /**
   * Ensures that at least one of the elements that the Locator points to,
   * is an attached and visible DOM node.
   */
  async toExist(locator: Locator) {
    // Counter-intuitive, but correct:
    // https://playwright.dev/docs/api/class-locatorassertions#locator-assertions-to-be-visible
    const assertionName = 'toExist'
    let pass: boolean
    try {
      await expect(locator.first()).toBeVisible()
      pass = true
    } catch (e) {
      console.log(e)
      pass = false
    }

    const message = () =>
      this.utils.matcherHint(assertionName, locator, '', {
        isNot: this.isNot,
      })

    return {
      message,
      pass,
      name: assertionName,
    }
  },

  async toBeSelected(locator: Locator) {
    const assertionName = 'toBeSelected'
    let pass: boolean
    try {
      await baseExpect(locator).toHaveClass(/(?<=^| )selected(?=$| )/, { timeout: 50 })
      pass = true
    } catch {
      // Do not log the error.
      pass = false
    }

    const message = () =>
      this.utils.matcherHint(assertionName, locator, '', {
        isNot: this.isNot,
      })

    return {
      message,
      pass,
      name: assertionName,
    }
  },
})
