/** @file Defines the React provider for the {@link Logger} interface, along with a hook to use the
 * provider via the shared React context. */
import * as react from "react";

// ==============
// === Logger ===
// ==============

/** Interface used to log logs, errors, etc.
 *
 * In the browser, this is the `Console` interface. In Electron, this is the `Logger` interface
 * provided by the EnsoGL packager. */
export interface Logger {
  /** Logs a message to the console. */
  log: (message: unknown, ...optionalParams: unknown[]) => void;
  /** Logs an error message to the console. */
  error: (message: unknown, ...optionalParams: unknown[]) => void;
}

// =====================
// === LoggerContext ===
// =====================

/** See {@link AuthContext} for safety details. */
// eslint-disable-next-line no-restricted-syntax
const LoggerContext = react.createContext<Logger>({} as Logger);

// ======================
// === LoggerProvider ===
// ======================

interface LoggerProviderProps {
  children: react.ReactNode;
  logger: Logger;
}

export function LoggerProvider(props: LoggerProviderProps) {
  const { children, logger } = props;
  return (
    <LoggerContext.Provider value={logger}>{children}</LoggerContext.Provider>
  );
}

// =================
// === useLogger ===
// =================

export function useLogger() {
  return react.useContext(LoggerContext);
}
