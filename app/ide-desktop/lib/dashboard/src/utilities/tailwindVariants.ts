/** @file `tailwind-variants` with a custom configuration. */
import * as tailwindVariants from 'tailwind-variants'

import * as tailwindMerge from '#/utilities/tailwindMerge'

export * from 'tailwind-variants'

// ==========
// === tv ===
// ==========

// This is a function, even though it does not contain function syntax.
// eslint-disable-next-line no-restricted-syntax
export const tv = tailwindVariants.createTV({ twMergeConfig: tailwindMerge.TAILWIND_MERGE_CONFIG })
