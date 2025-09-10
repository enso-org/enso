/** @file Utilities for working with URLs. */

/**
 * Checks if a URL is absolute.
 * @param url - The URL to check.
 * @returns True if the URL is absolute, false otherwise.
 */
export function isAbsoluteUrl(url: string) {
  try {
    new URL(url)
    return true
  } catch {
    return false
  }
}

export function isExternalLink(url: string) {
  return new URL(url, location.href).host !== location.host
}
