/** Save version of {@link URL.parse} that works in tests. */
export function urlParse(url: string | URL, base?: string | URL): URL | null {
  // We can't use URL.parse directly, because jsdom overwrites it with custom implementation,
  // and it causes `parsedUrl instanceof URL` checks to incorrectly return `false`. We have to
  // use the constructor to avoid this.
  try {
    return new URL(url, base)
  } catch {
    return null
  }
}
