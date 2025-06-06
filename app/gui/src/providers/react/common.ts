import { assert } from '@/util/assert'
import * as react from 'react'
import { Ref, toValue, watch } from 'vue'

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
 *
 * Returns also a setter if selector returns vue's Ref.
 */
export function useVueValue<T>(selector: () => T): T {
  const selectorCb = react.useCallback(selector, [])
  const initialValue = selectorCb()
  const [state, setState] = react.useState(initialValue)

  react.useEffect(
    () =>
      watch(
        () => selectorCb(),
        (newValue) => {
          setState(newValue)
        },
      ),
    [selectorCb, setState],
  )
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
