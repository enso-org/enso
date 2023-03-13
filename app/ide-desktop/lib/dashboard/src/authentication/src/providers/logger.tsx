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

type LoggerContextType = Logger;

// eslint-disable-next-line @typescript-eslint/naming-convention
const LoggerContext = react.createContext<LoggerContextType>(
  {} as LoggerContextType
);

// ======================
// === LoggerProvider ===
// ======================

interface LoggerProviderProps {
  children: react.ReactNode;
  logger: Logger;
}

// eslint-disable-next-line @typescript-eslint/naming-convention
export const LoggerProvider = (props: LoggerProviderProps) => {
  const { children, logger } = props;
  return (
    <LoggerContext.Provider value={logger}>{children}</LoggerContext.Provider>
  );
};

// =================
// === useLogger ===
// =================

export const useLogger = () => react.useContext(LoggerContext);
