/** @file The base class from which all `Actions` classes are derived. */

import * as test from '@playwright/test'

// ====================
// === PageCallback ===
// ====================

/** A callback that performs actions on a {@link test.Page}. */
export interface PageCallback {
  (input: test.Page): Promise<void> | void
}

// =======================
// === LocatorCallback ===
// =======================

/** A callback that performs actions on a {@link test.Locator}. */
export interface LocatorCallback {
  (input: test.Locator): Promise<void> | void
}

// ===================
// === BaseActions ===
// ===================

/** The base class from which all `Actions` classes are derived.
 * It contains method common to all `Actions` subclasses.
 * This is a [`thenable`], so it can be used as if it was a {@link Promise}.
 *
 * [`thenable`]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise#thenables
 */
export default class BaseActions implements PromiseLike<void> {
  /** Create a {@link BaseActions}. */
  constructor(
    protected readonly page: test.Page,
    private readonly promise = Promise.resolve()
  ) {}

  /** Press a key, replacing the text `Mod` with `Meta` (`Cmd`) on macOS, and `Control`
   * on all other platforms. */
  static press(page: test.Page, keyOrShortcut: string): Promise<void> {
    return test.test.step(`Press '${keyOrShortcut}'`, async () => {
      if (/\bMod\b|\bDelete\b/.test(keyOrShortcut)) {
        let userAgent = ''
        await test.test.step('Detect browser OS', async () => {
          userAgent = await page.evaluate(() => navigator.userAgent)
        })
        const isMacOS = /\bMac OS\b/i.test(userAgent)
        const ctrlKey = isMacOS ? 'Meta' : 'Control'
        const deleteKey = isMacOS ? 'Backspace' : 'Delete'
        const shortcut = keyOrShortcut.replace(/\bMod\b/, ctrlKey).replace(/\bDelete\b/, deleteKey)
        await page.keyboard.press(shortcut)
      } else {
        await page.keyboard.press(keyOrShortcut)
      }
    })
  }

  /** Proxies the `then` method of the internal {@link Promise}. */
  async then<T, E>(
    // The following types are copied almost verbatim from the type definitions for `Promise`.
    // eslint-disable-next-line no-restricted-syntax
    onfulfilled?: (() => PromiseLike<T> | T) | null | undefined,
    // eslint-disable-next-line no-restricted-syntax
    onrejected?: ((reason: unknown) => E | PromiseLike<E>) | null | undefined
  ) {
    return await this.promise.then(onfulfilled, onrejected)
  }

  /** Proxies the `catch` method of the internal {@link Promise}.
   * This method is not required for this to be a `thenable`, but it is still useful
   * to treat this class as a {@link Promise}. */
  // The following types are copied almost verbatim from the type definitions for `Promise`.
  // eslint-disable-next-line no-restricted-syntax
  async catch<T>(onrejected?: ((reason: unknown) => T) | null | undefined) {
    return await this.promise.catch(onrejected)
  }

  /** Return a {@link BaseActions} with the same {@link Promise} but a different type. */
  into<T extends new (page: test.Page, promise: Promise<void>) => InstanceType<T>>(
    clazz: T
  ): InstanceType<T> {
    return new clazz(this.page, this.promise)
  }

  /** Perform an action on the current page. This should generally be avoided in favor of using
   * specific methods; this is more or less an escape hatch used ONLY when the methods do not
   * support desired functionality. */
  do(callback: PageCallback): this {
    // @ts-expect-error This is SAFE, but only when the constructor of this class has the exact
    // same parameters as `BaseActions`.
    // eslint-disable-next-line @typescript-eslint/no-unsafe-return
    return new this.constructor(
      this.page,
      this.then(() => callback(this.page))
    )
  }

  /** Perform an action on the current page. */
  step(name: string, callback: PageCallback): this {
    return this.do(() => test.test.step(name, () => callback(this.page)))
  }

  /** Press a key, replacing the text `Mod` with `Meta` (`Cmd`) on macOS, and `Control`
   * on all other platforms. */
  press(keyOrShortcut: string): this {
    return this.do(page => BaseActions.press(page, keyOrShortcut))
  }
}
