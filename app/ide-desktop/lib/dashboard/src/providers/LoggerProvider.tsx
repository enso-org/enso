/** @file The React provider for the {@link Logger} interface, along with a hook to use the
 * provider via the shared React context. */
import * as loggerProvider from 'react'

// ==============
// === Logger ===
// ==============

/** Interface used to log logs, errors, etc.
 *
 * In the browser, this is the `Console` interface. In Electron, this is the `Logger` interface
 * provided by the EnsoGL packager. */
export interface Logger {
  /** Log a message to the console. */
  readonly log: (message: unknown, ...optionalParams: unknown[]) => void
  /** Log an error message to the console. */
  readonly error: (message: unknown, ...optionalParams: unknown[]) => void
}

// =====================
// === LoggerContext ===
// =====================

/** See `AuthContext` for safety details. */
// eslint-disable-next-line no-restricted-syntax
const LoggerContext = loggerProvider.createContext<Logger>({} as Logger)

// ======================
// === LoggerProvider ===
// ======================

/** Props for a {@link LoggerProvider}. */
export interface LoggerProviderProps {
  readonly children: loggerProvider.ReactNode
  readonly logger: Logger
}

/** A React provider containing the diagnostic logger. */
export default function LoggerProvider(props: LoggerProviderProps) {
  const { children, logger } = props
  return <LoggerContext.Provider value={logger}>{children}</LoggerContext.Provider>
}

// =================
// === useLogger ===
// =================

/** A React context hook exposing the diagnostic logger. */
export function useLogger() {
  return loggerProvider.useContext(LoggerContext)
}
