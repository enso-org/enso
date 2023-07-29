/** @file Module containing common custom React hooks used throughout out Dashboard. */
import * as React from 'react'
import * as router from 'react-router'
import * as toastify from 'react-toastify'

import * as app from './components/app'
import * as auth from './authentication/providers/auth'
import * as errorModule from './error'
import * as loggerProvider from './providers/logger'

// ==================
// === useRefresh ===
// ==================

/** An alias to make the purpose of the returned empty object clearer. */
export interface RefreshState {}

/** A hook that contains no state, and is used only to tell React when to re-render. */
export function useRefresh() {
    // Uses an empty object literal because every distinct literal
    // is a new reference and therefore is not equal to any other object literal.
    return React.useReducer((): RefreshState => ({}), {})
}

// ======================
// === useToastAndLog ===
// ======================

/** Return a function to send a toast with rendered error message. The same message is also logged
 * as an error. */
export function useToastAndLog() {
    const logger = loggerProvider.useLogger()
    return React.useCallback(
        <T,>(
            messagePrefix: string,
            error?: errorModule.MustNotBeKnown<T>,
            options?: toastify.ToastOptions
        ) => {
            const message =
                error == null
                    ? `${messagePrefix}.`
                    : // DO NOT explicitly pass the generic parameter anywhere else.
                      // It is only being used here because this function also checks for
                      // `MustNotBeKnown<T>`.
                      `${messagePrefix}: ${errorModule.getMessageOrToString<unknown>(error)}`
            const id = toastify.toast.error(message, options)
            logger.error(message)
            return id
        },
        [/* should never change */ logger]
    )
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
 * @param asyncEffect - The asynchronous function used to load the state controlled by this hook.
 * @param deps - The list of dependencies that, when updated, trigger the asynchronous effect.
 * @returns The current value of the state controlled by this hook. */
export function useAsyncEffect<T>(
    initialValue: T,
    asyncEffect: (signal: AbortSignal) => Promise<T>,
    deps?: React.DependencyList
): T {
    const logger = loggerProvider.useLogger()
    const [value, setValue] = React.useState<T>(initialValue)

    React.useEffect(() => {
        const controller = new AbortController()
        void asyncEffect(controller.signal).then(
            result => {
                if (!controller.signal.aborted) {
                    setValue(result)
                }
            },
            error => {
                logger.error('Error while fetching data:', error)
            }
        )
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

// =======================
// === Reactive Events ===
// =======================

/** A map containing all known event types. Names MUST be chosen carefully to avoid conflicts.
 * The simplest way to achieve this is by namespacing names using a prefix. */
export interface KnownEventsMap {}

/** A union of all known events. */
type KnownEvent = KnownEventsMap[keyof KnownEventsMap]

/** A wrapper around `useState` that calls `flushSync` after every `setState`.
 * This is required so that no events are dropped. */
export function useEvent<T extends KnownEvent>(): [events: T[], dispatchEvent: (event: T) => void] {
    const [events, setEvents] = React.useState<T[]>([])
    React.useEffect(() => {
        if (events.length !== 0) {
            setEvents([])
        }
    }, [events])
    const dispatchEvent = React.useCallback(
        (innerEvent: T) => {
            setEvents([...events, innerEvent])
        },
        [events]
    )
    return [events, dispatchEvent]
}

/** A wrapper around `useEffect` that has `event` as its sole dependency. */
export function useEventHandler<T extends KnownEvent>(
    events: T[],
    effect: (event: T) => Promise<void> | void
) {
    let hasEffectRun = false
    React.useLayoutEffect(() => {
        if (IS_DEV_MODE) {
            if (hasEffectRun) {
                // This is the second time this event is being run in React Strict Mode.
                // Event handlers are not supposed to be idempotent, so it is a mistake to execute it
                // a second time.
                // eslint-disable-next-line no-restricted-syntax
                return
            } else {
                // eslint-disable-next-line react-hooks/exhaustive-deps
                hasEffectRun = true
            }
        }
        void (async () => {
            for (const event of events) {
                await effect(event)
            }
        })()
    }, [events])
}

// =========================================
// === Debug wrappers for built-in hooks ===
// =========================================

// `console.*` is allowed because these are for debugging purposes only.
/* eslint-disable no-restricted-properties */

// === useDebugState ===

// `console.*` is allowed because this is for debugging purposes only.
/* eslint-disable no-restricted-properties */

/** A modified `useState` that logs the old and new values when `setState` is called. */
export function useDebugState<T>(
    initialState: T | (() => T),
    name?: string
): [state: T, setState: (valueOrUpdater: React.SetStateAction<T>, source?: string) => void] {
    const [state, rawSetState] = React.useState(initialState)

    const description = name != null ? `state for '${name}'` : 'state'

    const setState = React.useCallback(
        (valueOrUpdater: React.SetStateAction<T>, source?: string) => {
            const fullDescription = `${description}${source != null ? ` from '${source}'` : ''}`
            rawSetState(oldState => {
                const newState =
                    typeof valueOrUpdater === 'function'
                        ? // This is UNSAFE when `T` is itself a function type,
                          // however React makes the same assumption.
                          // eslint-disable-next-line no-restricted-syntax
                          (valueOrUpdater as (prevState: T) => T)(oldState)
                        : valueOrUpdater
                if (!Object.is(oldState, newState)) {
                    console.group(description)
                    console.trace(description)
                    console.log(`Old ${fullDescription}:`, oldState)
                    console.log(`New ${fullDescription}:`, newState)
                    console.groupEnd()
                }
                return newState
            })
        },
        [description]
    )

    return [state, setState]
}

// === useMonitorDependencies ===

/** A helper function to log the old and new values of changed dependencies. */
function useMonitorDependencies(
    dependencies: React.DependencyList,
    description?: string,
    dependencyDescriptions?: readonly string[]
) {
    const oldDependenciesRef = React.useRef(dependencies)
    const indicesOfChangedDependencies = dependencies.flatMap((dep, i) =>
        Object.is(dep, oldDependenciesRef.current[i]) ? [] : [i]
    )
    if (indicesOfChangedDependencies.length !== 0) {
        const descriptionText = description == null ? '' : `for '${description}'`
        console.group(`dependencies changed${descriptionText}`)
        for (const i of indicesOfChangedDependencies) {
            console.group(dependencyDescriptions?.[i] ?? `dependency #${i + 1}`)
            console.log('old value:', oldDependenciesRef.current[i])
            console.log('new value:', dependencies[i])
            console.groupEnd()
        }
        console.groupEnd()
    }
    oldDependenciesRef.current = dependencies
}

/* eslint-enable no-restricted-properties */

// === useDebugEffect ===

/** A modified `useEffect` that logs the old and new values of changed dependencies. */
export function useDebugEffect(
    effect: React.EffectCallback,
    deps: React.DependencyList,
    description?: string,
    dependencyDescriptions?: readonly string[]
) {
    useMonitorDependencies(deps, description, dependencyDescriptions)
    // eslint-disable-next-line react-hooks/exhaustive-deps
    React.useEffect(effect, deps)
}

// === useDebugMemo ===

/** A modified `useMemo` that logs the old and new values of changed dependencies. */
export function useDebugMemo<T>(
    factory: () => T,
    deps: React.DependencyList,
    description?: string,
    dependencyDescriptions?: readonly string[]
) {
    useMonitorDependencies(deps, description, dependencyDescriptions)
    // eslint-disable-next-line react-hooks/exhaustive-deps
    return React.useMemo<T>(factory, deps)
}

// === useDebugCallback ===

/** A modified `useCallback` that logs the old and new values of changed dependencies. */
export function useDebugCallback<T extends (...args: never[]) => unknown>(
    callback: T,
    deps: React.DependencyList,
    description?: string,
    dependencyDescriptions?: readonly string[]
) {
    useMonitorDependencies(deps, description, dependencyDescriptions)
    // eslint-disable-next-line react-hooks/exhaustive-deps
    return React.useCallback<T>(callback, deps)
}

/* eslint-enable no-restricted-properties */
