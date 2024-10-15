/** @file Google Analytics tag. */
import * as load from './load'

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
export function gtag(_action: 'config' | 'event' | 'js' | 'set', ..._args: unknown[]) {
  // @ts-expect-error This is explicitly not given types as it is a mistake to acess this
  // anywhere else.
  // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
  window.dataLayer.push([_action, ..._args])
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
