/**
 * @file
 *
 * Provides an HTTP client to the application.
 */
import * as React from 'react'

import HttpClient from '#/utilities/HttpClient'

const HTTPClientContext = React.createContext<HttpClient>(new HttpClient())

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
// eslint-disable-next-line react-refresh/only-export-components
export function useHttpClient() {
  return React.useContext(HTTPClientContext)
}
