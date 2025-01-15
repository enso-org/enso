/** @file `tailwind-variants` with a custom configuration. */
import type { OmitUndefined } from 'tailwind-variants'
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

/**
 * Props for a component that uses `tailwind-variants`.
 *
 * TODO: @MrFlashAccount [add support for styling individual slots](https://github.com/enso-org/cloud-v2/issues/1643)
 */
export type VariantProps<
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  Component extends (...args: any) => any,
> = Omit<OmitUndefined<Parameters<Component>[0]>, 'class' | 'className'> & {
  variants?: ExtractFunction<Component> | undefined
}
