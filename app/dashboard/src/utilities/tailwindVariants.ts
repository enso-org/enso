/** @file `tailwind-variants` with a custom configuration. */
import { createTV } from 'tailwind-variants'

import { TAILWIND_MERGE_CONFIG } from '#/utilities/tailwindMerge'

export * from 'tailwind-variants'

// ==========
// === tv ===
// ==========

// This is a function, even though it does not contain function syntax.
// eslint-disable-next-line no-restricted-syntax
export const tv = createTV({ twMergeConfig: TAILWIND_MERGE_CONFIG })

/** Extract function signatures from a type. */
export type ExtractFunction<T> =
  T extends (...args: infer Args) => infer Ret ? (...args: Args) => Ret : never

/** A `tailwind-variants` type, without restrictions onn the `extends` key. */
export type TVWithoutExtends<T> = ExtractFunction<T> & Omit<T, 'extend'>
