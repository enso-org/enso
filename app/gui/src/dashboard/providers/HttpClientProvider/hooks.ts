/** @file Hooks for `HttpClientProvider`. */
import { useContext } from 'react'
import { HTTPClientContext } from './constants'

/** Returns the HTTP client. */
export function useHttpClient() {
  return useContext(HTTPClientContext)
}
