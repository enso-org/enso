/** @file Provides an HTTP client to the application. */
import type { HttpClient } from '#/utilities/HttpClient'
import { type PropsWithChildren } from 'react'
import { HTTPClientContext } from './constants'

/** Props for an {@link HttpClientProvider}. */
export interface HttpClientProviderProps extends PropsWithChildren {
  readonly httpClient: HttpClient
}

/**
 * Provides an HTTP client to the application.
 * Use this provider to inject an HTTP client into the application and fetch data.
 */
export function HttpClientProvider(props: HttpClientProviderProps) {
  const { children, httpClient } = props
  return <HTTPClientContext.Provider value={httpClient}>{children}</HTTPClientContext.Provider>
}
