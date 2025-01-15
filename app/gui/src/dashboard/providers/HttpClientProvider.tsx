/**
 * @file
 *
 * Provides an HTTP client to the application.
 */
import * as React from 'react'

import invariant from 'tiny-invariant'

import type HttpClient from '#/utilities/HttpClient'

const HTTPClientContext = React.createContext<HttpClient | null>(null)

/** Props for an {@link HttpClientProvider}. */
export interface HttpClientProviderProps extends React.PropsWithChildren {
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

/** Returns the HTTP client. */
export function useHttpClient() {
  return React.useContext(HTTPClientContext)
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
