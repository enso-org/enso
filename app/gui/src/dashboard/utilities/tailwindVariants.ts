/** @file `tailwind-variants` with a custom configuration. */
import type { OmitUndefined } from 'tailwind-variants'
import { createTV } from 'tailwind-variants'

import { TAILWIND_MERGE_CONFIG } from '#/utilities/tailwindMerge'
import { LRUCache } from './LruCache'

export * from 'tailwind-variants'

const MAX_CACHE_SIZE = 256

// eslint-disable-next-line no-restricted-syntax
const tvConstructor = createTV({ twMergeConfig: TAILWIND_MERGE_CONFIG })

// This is a function, even though it does not contain function syntax.
// eslint-disable-next-line no-restricted-syntax
export const tv: typeof tvConstructor = function tvWithLRU(
  construct: Parameters<typeof tvConstructor>[0],
) {
  const cache = new LRUCache<string, { value: unknown }>(MAX_CACHE_SIZE)
  /**
   * Get a cache key for a given set of arguments.
   */
  function getCacheKey(props: Parameters<typeof baseVariants>[0]) {
    return JSON.stringify(props)
  }

  const baseVariants = tvConstructor(construct)

  /**
   * Variants constructor with LRU cache.
   */
  function variantsWithLRU(args: Parameters<typeof baseVariants>[0]) {
    const cacheKey = getCacheKey(args)
    const cached = cache.get(cacheKey)

    if (cached != null) {
      return cached.value
    }

    const result: unknown = baseVariants(args)

    if (typeof result === 'object' && result != null) {
      for (const slot in result) {
        // eslint-disable-next-line no-restricted-syntax
        const value = result[slot as keyof typeof result]

        if (typeof value === 'function') {
          const slotCachePrefix = slot + cacheKey
          /**
           * Wrap a slot function with a cache.
           */
          // @ts-expect-error - This is a valid assignment.
          result[slot] = function withSlotCache(props: Parameters<typeof value>[0]) {
            const slotCacheKey = slotCachePrefix + getCacheKey(props)
            const slotCache = cache.get(slotCacheKey)

            if (slotCache != null) {
              return slotCache.value
            }

            // @ts-expect-error - This is a valid assignment.
            // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
            const classes = value(props)

            cache.set(slotCacheKey, { value: classes })

            // eslint-disable-next-line @typescript-eslint/no-unsafe-return
            return classes
          }
        }
      }
    }

    cache.set(cacheKey, { value: result })
    return result
  }
  // Extend the prototype of the `variantsWithLRU` function with the `baseVariants` function.
  // This is done to preserve the extra properties of the `baseVariants` function.
  variantsWithLRU.__proto__ = baseVariants

  // eslint-disable-next-line no-restricted-syntax
  return variantsWithLRU as unknown as typeof tvConstructor
} as unknown as typeof tvConstructor

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
  /**
   * Custom styles for a component.
   *
   * You can use this to override the default styles for a component.
   * @example
   * ```tsx
   * const COMPONENT_STYLES = tv({
   *   base: 'block',
   *   slots: {
   *     root: 'bg-red-500',
   *   },
   * })
   *
   * const OVERRIDES = tv({
   *   extend: COMPONENT_STYLES,
   *   slots: {
   *     // overrides the slot bg, but keeps the base styles
   *     root: 'bg-blue-500',
   *   },
   * })
   *
   * <MyComponent variants={OVERRIDES} />
   * ```
   */
  variants?: ExtractFunction<Component> | undefined
}
