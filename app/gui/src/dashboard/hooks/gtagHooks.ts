/** @file A hook that makes `gtag.event()` a no-op if the user is offline. */
import * as load from 'enso-common/src/load'
import * as React from 'react'

const GOOGLE_ANALYTICS_TAG = $config.GOOGLE_ANALYTICS_TAG

if (GOOGLE_ANALYTICS_TAG != null) {
  void load.loadScript(`https://www.googletagmanager.com/gtag/js?id=${GOOGLE_ANALYTICS_TAG}`)
}

// @ts-expect-error This is explicitly not given types as it is a mistake to acess this
// anywhere else.
// eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/strict-boolean-expressions
window.dataLayer = window.dataLayer || []

/** Google Analytics tag function. */
// eslint-disable-next-line @typescript-eslint/no-unused-vars
function gtag(action: 'config' | 'event' | 'js' | 'set', ...args: unknown[]) {
  // @ts-expect-error This is explicitly not given types as it is a mistake to acess this
  // anywhere else.
  // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
  window.dataLayer.push([action, ...args])
}

/** Send event to Google Analytics. */
function event(name: string, params?: object) {
  gtag('event', name, params)
}

gtag('js', new Date())
// eslint-disable-next-line camelcase, @typescript-eslint/naming-convention
gtag('set', 'linker', { accept_incoming: true })
gtag('config', GOOGLE_ANALYTICS_TAG)
if (GOOGLE_ANALYTICS_TAG === 'G-CLTBJ37MDM') {
  gtag('config', 'G-DH47F649JC')
}

// ====================
// === useGtagEvent ===
// ====================

/**
 * A hook that returns a no-op if the user is offline, otherwise it returns
 * a transparent wrapper around `gtag.event`.
 */
export function useGtagEvent() {
  return React.useCallback((name: string, params?: object) => {
    event(name, params)
  }, [])
}

// =============================
// === gtagOpenCloseCallback ===
// =============================

/**
 * Send an event indicating that something has been opened, and return a cleanup function
 * sending an event indicating that it has been closed.
 *
 * Also sends the close event when the window is unloaded.
 */
export function gtagOpenCloseCallback(
  gtagEvent: ReturnType<typeof useGtagEvent>,
  openEvent: string,
  closeEvent: string,
) {
  gtagEvent(openEvent)

  const onBeforeUnload = () => {
    gtagEvent(closeEvent)
  }
  window.addEventListener('beforeunload', onBeforeUnload)

  return () => {
    window.removeEventListener('beforeunload', onBeforeUnload)
    gtagEvent(closeEvent)
  }
}
