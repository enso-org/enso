/** @file A collection of generic utility functions. */
import { identity } from '@vueuse/core'

/** A stable reference to a function that does nothing. */
// eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-explicit-any
export const noop: (...args: any[]) => void = () => {}
// eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-explicit-any
export const noopPromise: (...args: any[]) => Promise<void> = () => Promise.resolve()

/** A stable reference to a function that returns its input. */
export { identity }
