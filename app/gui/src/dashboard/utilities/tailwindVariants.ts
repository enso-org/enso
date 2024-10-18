/** @file `tailwind-variants` with a custom configuration. */
import type { VariantProps as TvVariantProps } from 'tailwind-variants'
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

/** A `tailwind-variants` type, without restrictions on the `extends` key. */
export type TVWithoutExtends<T> = ExtractFunction<T> & Omit<T, 'extend'>

/** Props for a component that uses `tailwind-variants`. */
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export type VariantProps<T extends (...args: any) => any> = Omit<
  TvVariantProps<T>,
  'class' | 'className'
> & {
  variants?: ExtractFunction<T> | undefined
}
