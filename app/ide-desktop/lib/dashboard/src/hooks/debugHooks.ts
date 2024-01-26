/** @file Module containing common custom React hooks used throughout out Dashboard. */
import * as React from 'react'

// =========================================
// === Debug wrappers for built-in hooks ===
// =========================================

// `console.*` is allowed because these are for debugging purposes only.
/* eslint-disable no-restricted-properties */

// === useDebugState ===

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
