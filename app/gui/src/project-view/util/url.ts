import { Err, Ok, type Result } from 'enso-common/src/utilities/data/result'

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

export interface DeeplinkUrl {
  readonly pathname: string
  readonly searchParams: URLSearchParams
  readonly search: string
}

/**
 * Parse an `enso:` deeplink URL.
 */
export function parseEnsoDeeplink(url: URL | string): Result<DeeplinkUrl> {
  const parsedUrl = typeof url === 'string' ? urlParse(url) : url
  if (parsedUrl == null || parsedUrl.protocol !== 'enso:') return Err('Invalid deeplink URL')
  const trimmedPath = parsedUrl.pathname.slice(parsedUrl.pathname[0] === '/' ? 1 : 0)
  const pathname = [parsedUrl.host, trimmedPath].filter(Boolean).join('/')
  return Ok({
    pathname,
    searchParams: parsedUrl.searchParams,
    search: parsedUrl.searchParams.size > 0 ? `?${parsedUrl.searchParams}` : '',
  })
}
