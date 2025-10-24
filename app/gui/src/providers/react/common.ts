import { assert } from '@/util/assert'
import * as react from 'react'
import { toValue, watch, type Ref, type WatchSource } from 'vue'

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

/**
 * Use Vue value in react reactively.
 *
 * The selector vue's reactive dependencies are tracked, and the React component is re-rendered
 * when the value changed.
 */
export function useVueValue<T>(selector: WatchSource<T>, deep = false): T {
  const [state, setState] = react.useState(() => toValue<T>(selector))
  react.useEffect(() => {
    return watch(
      selector,
      (newValue) => {
        setState(newValue)
      },
      // We need to set state synchronously to make react transitions working properly.
      { flush: 'sync', deep, immediate: true },
    )
  }, [selector, deep])
  return state
}

/**
 * Use Vue Ref in react reactively.
 *
 * Same as {@link useVueValue} but returns also a setter.
 */
export function useVueRef<T>(selector: () => Ref<T>): [T, (newVal: T) => void] {
  return [
    useVueValue(() => toValue(selector())),
    (newVal) => {
      selector().value = newVal
    },
  ]
}
