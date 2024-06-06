/** @file Return an always-up-to-date ref tracking a value returned by a callback. */
import * as React from 'react'

import * as store from '#/store'

// ===================
// === useStoreRef ===
// ===================

// NOTE: This MUST be a default export, because react-hooks/exhaustive-deps only understands bare
// identifiers, not ones accessed via a namespace, so `useStoreRef` works but `store.useStoreRef`
// does not.
/** Return an always-up-to-date ref tracking a value returned by a callback. */
export default function useStoreRef<T>(
  extractValue: (state: store.Store, prevState: store.Store) => T,
  dependencies: React.DependencyList
) {
  const extractValueRef = React.useRef(extractValue)
  extractValueRef.current = extractValue
  const [initialValue] = React.useState(() => {
    const storeState = store.useStore.getState()
    return extractValue(storeState, storeState)
  })
  const ref = React.useRef(initialValue)
  React.useEffect(
    () =>
      store.useStore.subscribe((state, prevState) => {
        ref.current = extractValueRef.current(state, prevState)
      }),
    // The dependency list passed into this hook is checked by ESLint as a custom hook.
    // eslint-disable-next-line react-hooks/exhaustive-deps
    dependencies
  )
  return ref
}
