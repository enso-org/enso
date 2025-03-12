/** @file Constants for `LoggerProvider`. */
import { createContext } from 'react'

/**
 * Interface used to log logs, errors, etc.
 *
 * In the browser, this is the `Console` interface. In Electron, this is the `Logger` interface
 * provided by the EnsoGL packager.
 */
export interface Logger {
  /** Log a message to the console. */
  readonly log: (message: unknown, ...optionalParams: unknown[]) => void
  /** Log an error message to the console. */
  readonly error: (message: unknown, ...optionalParams: unknown[]) => void
}

/** See `AuthContext` for safety details. */
export const LoggerContext = createContext<Logger>({ log: () => {}, error: () => {} })
