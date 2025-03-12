/** @file Hooks for `HttpClientProvider`. */
import { useContext } from 'react'
import invariant from 'tiny-invariant'
import { HTTPClientContext } from './constants'

/** Returns the HTTP client. */
export function useHttpClient() {
  return useContext(HTTPClientContext)
}

/**
 * Returns the HTTP client.
 * @throws If the HTTP client is not found in context.
 */
export function useHttpClientStrict() {
  const httpClient = useHttpClient()
  invariant(httpClient, 'HTTP client not found in context')
  return httpClient
}
