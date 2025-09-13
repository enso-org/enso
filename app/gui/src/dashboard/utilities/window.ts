/** @file Wrappers around functions related to `window`. */

/** Open a link in a new browser tab. */
export function openInNewBrowserTab(url: URL | string) {
  window.open(url, '_blank')
}
