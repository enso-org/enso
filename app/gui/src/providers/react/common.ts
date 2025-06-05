import { assert } from '@/util/assert'
import * as react from 'react'

/**
 * A helper for getting contexts where they are asserted to be provided with non-nullish
 * value
 */
export function useInReactFunction<T>(context: react.Context<T | null>) {
  return () => {
    const value = react.useContext(context)
    assert(value != null, "Context for React wasn't provided")
    return value
  }
}
