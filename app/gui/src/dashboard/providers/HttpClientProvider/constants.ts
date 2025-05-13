/** @file Constants for `HttpClientProvider`. */
import { HttpClient } from '#/utilities/HttpClient'
import { createContext } from 'react'

export const HTTPClientContext = createContext(new HttpClient())
