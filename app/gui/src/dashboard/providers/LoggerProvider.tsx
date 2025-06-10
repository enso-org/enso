/**
 * @file The React provider for the {@link Logger} interface, along with a hook to use the
 * provider via the shared React context.
 */
import * as React from 'react'

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
// eslint-disable-next-line no-restricted-syntax
const LoggerContext = React.createContext<Logger>({} as Logger)

/** Props for a {@link LoggerProvider}. */
export interface LoggerProviderProps {
  readonly children: React.ReactNode
  readonly logger: Logger
}

/** A React provider containing the diagnostic logger. */
export default function LoggerProvider(props: LoggerProviderProps) {
  const { children, logger } = props
  return <LoggerContext.Provider value={logger}>{children}</LoggerContext.Provider>
}

/** A React context hook exposing the diagnostic logger. */
// eslint-disable-next-line react-refresh/only-export-components
export function useLogger() {
  return React.useContext(LoggerContext)
}
