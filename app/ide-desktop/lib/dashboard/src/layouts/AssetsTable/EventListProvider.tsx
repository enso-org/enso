/** @file The React provider (and associated hooks) for providing reactive events. */
import * as React from 'react'

import invariant from 'tiny-invariant'
import * as zustand from 'zustand'

import type * as assetEvent from '#/events/assetEvent'
import type * as assetListEvent from '#/events/assetListEvent'

// ======================
// === EventListStore ===
// ======================

/** The state of this zustand store. */
interface EventListStore {
  readonly assetEvents: readonly assetEvent.AssetEvent[]
  readonly assetListEvents: readonly assetListEvent.AssetListEvent[]
  readonly dispatchAssetEvent: (event: assetEvent.AssetEvent) => void
  readonly dispatchAssetListEvent: (event: assetListEvent.AssetListEvent) => void
}

// ========================
// === EventListContext ===
// ========================

/** State contained in a `EventListContext`. */
export interface EventListContextType extends zustand.StoreApi<EventListStore> {}

const EventListContext = React.createContext<EventListContextType | null>(null)

/** Props for a {@link EventListProvider}. */
export interface EventListProviderProps extends Readonly<React.PropsWithChildren> {}

// =========================
// === EventListProvider ===
// =========================

/** A React provider (and associated hooks) for determining whether the current area
 * containing the current element is focused. */
export default function EventListProvider(props: EventListProviderProps) {
  const { children } = props
  const [store] = React.useState(() =>
    zustand.createStore<EventListStore>((set, get) => ({
      assetEvents: [],
      dispatchAssetEvent: event => {
        set({ assetEvents: [...get().assetEvents, event] })
      },
      assetListEvents: [],
      dispatchAssetListEvent: event => {
        set({ assetListEvents: [...get().assetListEvents, event] })
      },
    }))
  )

  React.useLayoutEffect(() => {
    store.setState({ assetEvents: [], assetListEvents: [] })
  })

  return <EventListContext.Provider value={store}>{children}</EventListContext.Provider>
}

// ====================
// === useEventList ===
// ====================

/** Functions for getting and setting the event list. */
function useEventList() {
  const store = React.useContext(EventListContext)

  invariant(store, 'Event list store can only be used inside an `EventListProvider`.')

  return store
}

// =============================
// === useDispatchAssetEvent ===
// =============================

/** A function to add a new reactive event. */
export function useDispatchAssetEvent() {
  return zustand.useStore(useEventList(), state => state.dispatchAssetEvent)
}

// =================================
// === useDispatchAssetListEvent ===
// =================================

/** A function to add a new reactive event. */
export function useDispatchAssetListEvent() {
  return zustand.useStore(useEventList(), state => state.dispatchAssetListEvent)
}

// =============================
// === useAssetEventListener ===
// =============================

/** Execute a callback for every new asset event. */
export function useAssetEventListener(
  callback: (event: assetEvent.AssetEvent) => Promise<void> | void
) {
  useEventList().subscribe(state => {
    for (const event of state.assetEvents) {
      void callback(event)
    }
  })
}

// =================================
// === useAssetListEventListener ===
// =================================

/** Execute a callback for every new asset list event. */
export function useAssetListEventListener(
  callback: (event: assetListEvent.AssetListEvent) => Promise<void> | void
) {
  useEventList().subscribe(state => {
    for (const event of state.assetListEvents) {
      void callback(event)
    }
  })
}
