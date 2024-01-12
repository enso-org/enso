/** @file Reactive events. */
import * as React from 'react'

import * as detect from 'enso-common/src/detect'

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
            // This must run after the current render, but before the next.
            queueMicrotask(() => {
                setEvents([])
            })
        }
    }, [events])
    const dispatchEvent = React.useCallback((event: T) => {
        setEvents(oldEvents => [...oldEvents, event])
    }, [])
    return [events, dispatchEvent]
}

/** A wrapper around `useEffect` that has `event` as its sole dependency. */
export function useEventHandler<T extends KnownEvent>(
    events: T[],
    effect: (event: T) => Promise<void> | void
) {
    let hasEffectRun = false
    React.useLayoutEffect(() => {
        if (detect.IS_DEV_MODE) {
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
