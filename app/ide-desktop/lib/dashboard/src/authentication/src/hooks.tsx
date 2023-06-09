/** @file Module containing common custom React hooks used throughout out Dashboard. */
import * as react from 'react'
import * as router from 'react-router'

import * as app from './components/app'
import * as auth from './authentication/providers/auth'
import * as loggerProvider from './providers/logger'

// ==================
// === useRefresh ===
// ==================

/** A hook that contains no state, and is used only to tell React when to re-render. */
export function useRefresh() {
    // Uses an empty object literal because every distinct literal
    // is a new reference and therefore is not equal to any other object literal.
    return react.useReducer(() => ({}), {})
}

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
 *
 * @param initialValue - The initial value of the state controlled by this hook.
 * @param fetch - The asynchronous function used to load the state controlled by this hook.
 * @param deps - The list of dependencies that, when updated, trigger the asynchronous fetch.
 * @returns The current value of the state controlled by this hook. */
export function useAsyncEffect<T>(
    initialValue: T,
    fetch: (signal: AbortSignal) => Promise<T>,
    deps?: react.DependencyList
): T {
    const logger = loggerProvider.useLogger()
    const [value, setValue] = react.useState<T>(initialValue)

    react.useEffect(() => {
        const controller = new AbortController()
        const { signal } = controller

        /** Declare the async data fetching function. */
        const load = async () => {
            const result = await fetch(signal)

            /** Set state with the result only if this effect has not been aborted. This prevents race
             * conditions by making it so that only the latest async fetch will update the state on
             * completion. */
            if (!signal.aborted) {
                setValue(result)
            }
        }

        load().catch(error => {
            logger.error('Error while fetching data', error)
        })
        /** Cancel any future `setValue` calls. */
        return () => {
            controller.abort()
        }
    }, deps)

    return value
}

// ===================
// === useNavigate ===
// ===================

/** A wrapper around {@link router.useNavigate} that goes into offline mode when
 * offline. */
export function useNavigate() {
    const { goOffline } = auth.useAuth()
    // This function is a wrapper around `router.useNavigate`. It shouldbe the only place where
    // `router.useNavigate` is used.
    // eslint-disable-next-line no-restricted-properties
    const originalNavigate = router.useNavigate()

    const navigate: router.NavigateFunction = (...args: [unknown, unknown?]) => {
        const isOnline = navigator.onLine
        if (!isOnline) {
            void goOffline()
            originalNavigate(app.DASHBOARD_PATH)
        } else {
            // This is safe, because the arguments are being passed through transparently.
            // eslint-disable-next-line no-restricted-syntax
            originalNavigate(...(args as [never, never?]))
        }
    }

    return navigate
}

// =====================
// === useDebugState ===
// =====================

/** A modified `useState` that logs the old and new values when `setState` is called. */
export function useDebugState<T>(
    initialState: T | (() => T),
    name?: string
): [state: T, setState: (valueOrUpdater: react.SetStateAction<T>, source?: string) => void] {
    const [state, rawSetState] = react.useState(initialState)

    const description = name != null ? `state for '${name}'` : 'state'

    const setState = react.useCallback(
        (valueOrUpdater: react.SetStateAction<T>, source?: string) => {
            const fullDescription = `${description}${source != null ? ` from '${source}'` : ''}`
            if (typeof valueOrUpdater === 'function') {
                // This is UNSAFE, however React makes the same assumption.
                // eslint-disable-next-line no-restricted-syntax
                const updater = valueOrUpdater as (prevState: T) => T
                // `console.*` is allowed because this is for debugging purposes only.
                /* eslint-disable no-restricted-properties */
                rawSetState(oldState => {
                    console.group(description)
                    console.log(`Old ${fullDescription}:`, oldState)
                    const newState = updater(oldState)
                    console.log(`New ${fullDescription}:`, newState)
                    console.groupEnd()
                    return newState
                })
            } else {
                rawSetState(oldState => {
                    if (!Object.is(oldState, valueOrUpdater)) {
                        console.group(description)
                        console.log(`Old ${fullDescription}:`, oldState)
                        console.log(`New ${fullDescription}:`, valueOrUpdater)
                        console.groupEnd()
                    }
                    return valueOrUpdater
                })
                /* eslint-enable no-restricted-properties */
            }
        },
        []
    )

    return [state, setState]
}
