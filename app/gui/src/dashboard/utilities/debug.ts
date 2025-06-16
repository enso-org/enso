/** @file Functions for debugging events. */

/** Log all events for an event target. */
export function debugEvents<
  T extends EventTarget &
    Partial<Record<keyof T & `on${string}`, ((event: never) => unknown) | null | undefined>>,
>(target: T, options?: AddEventListenerOptions, callback?: (event: Event) => void) {
  callback ??= (event) => {
    // This is allowed, since it is for debugging purposes only.
    // eslint-disable-next-line no-restricted-properties
    console.info('Event triggered', event.type, event)
  }
  const logEvent = (event: unknown) => {
    if (event instanceof Event) {
      callback(event)
    }
  }
  for (const key in target) {
    if (/^on/.test(key)) {
      target.addEventListener(key.slice(2), logEvent, options)
    }
  }
  return () => {
    for (const key in target) {
      if (/^on/.test(key)) {
        target.removeEventListener(key.slice(2), logEvent, options)
      }
    }
  }
}
