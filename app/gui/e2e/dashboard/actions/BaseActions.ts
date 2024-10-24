/** @file The base class from which all `Actions` classes are derived. */
import * as test from '@playwright/test'

import type * as inputBindings from '#/utilities/inputBindings'

import { modModifier } from '.'

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

/**
 * The base class from which all `Actions` classes are derived.
 * It contains method common to all `Actions` subclasses.
 * This is a [`thenable`], so it can be used as if it was a {@link Promise}.
 *
 * [`thenable`]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise#thenables
 */
export default class BaseActions implements Promise<void> {
  /** Create a {@link BaseActions}. */
  constructor(
    protected readonly page: test.Page,
    private readonly promise = Promise.resolve(),
  ) {}

  /**
   * Get the string name of the class of this instance. Required for this class to implement
   * {@link Promise}.
   */
  get [Symbol.toStringTag]() {
    return this.constructor.name
  }

  /**
   * Press a key, replacing the text `Mod` with `Meta` (`Cmd`) on macOS, and `Control`
   * on all other platforms.
   */
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
    onfulfilled?: (() => PromiseLike<T> | T) | null | undefined,
    onrejected?: ((reason: unknown) => E | PromiseLike<E>) | null | undefined,
  ) {
    return await this.promise.then(onfulfilled, onrejected)
  }

  /**
   * Proxies the `catch` method of the internal {@link Promise}.
   * This method is not required for this to be a `thenable`, but it is still useful
   * to treat this class as a {@link Promise}.
   */
  async catch<T>(onrejected?: ((reason: unknown) => PromiseLike<T> | T) | null | undefined) {
    return await this.promise.catch(onrejected)
  }

  /**
   * Proxies the `catch` method of the internal {@link Promise}.
   * This method is not required for this to be a `thenable`, but it is still useful
   * to treat this class as a {@link Promise}.
   */
  async finally(onfinally?: (() => void) | null | undefined): Promise<void> {
    await this.promise.finally(onfinally)
  }

  /** Return a {@link BaseActions} with the same {@link Promise} but a different type. */
  into<
    T extends new (page: test.Page, promise: Promise<void>, ...args: Args) => InstanceType<T>,
    Args extends readonly unknown[],
  >(clazz: T, ...args: Args): InstanceType<T> {
    return new clazz(this.page, this.promise, ...args)
  }

  /**
   * Perform an action on the current page. This should generally be avoided in favor of using
   * specific methods; this is more or less an escape hatch used ONLY when the methods do not
   * support desired functionality.
   */
  do(callback: PageCallback): this {
    // @ts-expect-error This is SAFE, but only when the constructor of this class has the exact
    // same parameters as `BaseActions`.
    return new this.constructor(
      this.page,
      this.then(() => callback(this.page)),
    )
  }

  /** Perform an action on the current page. */
  step(name: string, callback: PageCallback) {
    return this.do(() => test.test.step(name, () => callback(this.page)))
  }

  /**
   * Press a key, replacing the text `Mod` with `Meta` (`Cmd`) on macOS, and `Control`
   * on all other platforms.
   */
  press<Key extends string>(keyOrShortcut: inputBindings.AutocompleteKeybind<Key>) {
    return this.do((page) => BaseActions.press(page, keyOrShortcut))
  }

  /** Perform actions until a predicate passes. */
  retry(
    callback: (actions: this) => this,
    predicate: (page: test.Page) => Promise<boolean>,
    options: { retries?: number; delay?: number } = {},
  ) {
    const { retries = 3, delay = 1_000 } = options
    return this.step('Perform actions with retries', async (thePage) => {
      for (let i = 0; i < retries; i += 1) {
        await callback(this)
        if (await predicate(thePage)) {
          return
        }
        await thePage.waitForTimeout(delay)
      }
      throw new Error('This action did not succeed.')
    })
  }

  /** Perform actions with the "Mod" modifier key pressed. */
  withModPressed<R extends BaseActions>(callback: (actions: this) => R) {
    return callback(
      this.step('Press "Mod"', async (page) => {
        await page.keyboard.down(await modModifier(page))
      }),
    ).step('Release "Mod"', async (page) => {
      await page.keyboard.up(await modModifier(page))
    })
  }

  /**
   * Expect an input to have an error (or no error if the expected value is `null`).
   * If the expected value is `undefined`, the assertion is skipped.
   */
  expectInputError(testId: string, description: string, expected: string | null | undefined) {
    if (expected === undefined) {
      return this
    } else if (expected != null) {
      return this.step(`Expect ${description} error to be '${expected}'`, async (page) => {
        await test.expect(page.getByTestId(testId).getByTestId('error')).toHaveText(expected)
      })
    } else {
      return this.step(`Expect no ${description} error`, async (page) => {
        await test.expect(page.getByTestId(testId).getByTestId('error')).not.toBeVisible()
      })
    }
  }
}
