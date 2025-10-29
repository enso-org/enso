/** @file API for sending events to Google Analytics. */
import { noop } from '#/utilities/functions'
import { loadScript } from '$/utils/load'

const GOOGLE_ANALYTICS_TAG = typeof $config !== 'undefined' && $config.GOOGLE_ANALYTICS_TAG

if (GOOGLE_ANALYTICS_TAG) {
  void loadScript(`https://www.googletagmanager.com/gtag/js?id=${GOOGLE_ANALYTICS_TAG}`).catch(noop)
}

// @ts-expect-error This is explicitly not given types as it is a mistake to acess this
// anywhere else.
window.dataLayer = window.dataLayer || []

/** Google Analytics tag function. */
export function gtag(action: 'config' | 'event' | 'js' | 'set', ...args: unknown[]) {
  // @ts-expect-error This is explicitly not given types as it is a mistake to acess this
  // anywhere else.
  window.dataLayer.push([action, ...args])
}

/** Send event to Google Analytics. */
export function event(name: string, params?: object) {
  gtag('event', name, params)
}

gtag('js', new Date())
// eslint-disable-next-line camelcase
gtag('set', 'linker', { accept_incoming: true })
gtag('config', GOOGLE_ANALYTICS_TAG)
if (GOOGLE_ANALYTICS_TAG === 'G-CLTBJ37MDM') {
  gtag('config', 'G-DH47F649JC')
}

/**
 * Send an event indicating that something has been opened, and return a cleanup function
 * sending an event indicating that it has been closed.
 *
 * Also sends the close event when the window is unloaded.
 */
export function openCloseCallback(openEvent: string, closeEvent: string) {
  event(openEvent)

  const onBeforeUnload = () => {
    event(closeEvent)
  }
  window.addEventListener('beforeunload', onBeforeUnload)

  return () => {
    window.removeEventListener('beforeunload', onBeforeUnload)
    event(closeEvent)
  }
}
