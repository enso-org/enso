/** @file Asynchronous wrapper for {@link React.useEffect}. */
import * as React from 'react'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

// ======================
// === useAsyncEffect ===
// ======================

/** A React hook for re-rendering a component once an asynchronous call is over.
 *
 * This hook will take care of setting an initial value for the component state (so that it can
 * render immediately), updating the state once the asynchronous call is over (to re-render the
 * component), and cancelling any in-progress asynchronous calls when the component is unmounted (to
 * avoid race conditions where "update 1" starts, "update 2" starts and finishes, then "update 1"
 * finishes and sets the state).
 *
 * For further details, see: https://devtrium.com/posts/async-functions-useeffect.
 * Also see: https://stackoverflow.com/questions/61751728/asynchronous-calls-with-react-usememo.
 * @param initialValue - The initial value of the state controlled by this hook.
 * @param asyncEffect - The asynchronous function used to load the state controlled by this hook.
 * @param deps - The list of dependencies that, when updated, trigger the asynchronous effect.
 * @returns The current value of the state controlled by this hook. */
export function useAsyncEffect<T>(
  initialValue: T,
  asyncEffect: (signal: AbortSignal) => Promise<T>,
  deps?: React.DependencyList
): T {
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [value, setValue] = React.useState<T>(initialValue)

  React.useEffect(() => {
    const controller = new AbortController()
    void (async () => {
      try {
        const result = await asyncEffect(controller.signal)
        if (!controller.signal.aborted) {
          setValue(result)
        }
      } catch (error) {
        toastAndLog('Error while fetching data', error)
      }
    })()
    /** Cancel any future `setValue` calls. */
    return () => {
      controller.abort()
    }
    // This is a wrapper function around `useEffect`, so it has its own `deps` array.
    // `asyncEffect` is omitted as it always changes - this is intentional.
    // `logger` is omitted as it should not trigger the effect.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, deps ?? [])

  return value
}
