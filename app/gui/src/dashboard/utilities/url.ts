/** @file Utilities for working with URLs. */

/**
 * Check if a URL is absolute.
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

/**
 * Whether an absolute or relative URL is from a different domain.
 * Relative URLs should always be from the same domain, but they are supported here for convenience.
 */
export function isExternalLink(url: string) {
  return new URL(url, location.href).host !== location.host
}
