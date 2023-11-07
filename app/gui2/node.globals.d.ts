import type { Locator } from '@playwright/test'

declare global {
  namespace PlaywrightTest {
    const r: unique symbol
    const t: unique symbol
    interface Matchers<R, T> {
      [r]: R
      [t]: T
      toExist(this: Matchers<R, Locator>): Promise<void>
    }
  }
}
