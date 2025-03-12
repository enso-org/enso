/** @file Constants for `HttpClientProvider`. */
import type { HttpClient } from '#/utilities/HttpClient'
import { createContext } from 'react'

export const HTTPClientContext = createContext<HttpClient | null>(null)
