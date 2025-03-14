/** @file Constants for `UIProviders`. */
import { createContext } from 'react'

/** A context containing the root elements for the application. */
export interface RootContextType {
  readonly portalRoot: HTMLElement
  readonly appRoot: HTMLElement
}

export const RootContext = createContext<RootContextType>({
  portalRoot: document.body,
  appRoot: document.body,
})
