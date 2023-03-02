
/** @file Defines the React provider for the {@link Logger} interface, along with a hook to use the
 * provider via the shared React context. */
import * as React from "react";



// ==============
// === Logger ===
// ==============

/** Interface used to log logs, errors, etc.
 *
 * In the browser, this is the `Console` interface. In Electron, this is the `Logger` interface
 * provided by the EnsoGL packager. */
export interface Logger {
    /** Logs a message to the console. */
    log: (message?: any, ...optionalParams: any[]) => void;
    /** Logs an error message to the console. */
    error: (message?: any, ...optionalParams: any[]) => void;
}



// =====================
// === LoggerContext ===
// =====================

type LoggerContextType = Logger;

// eslint-disable-next-line @typescript-eslint/naming-convention
const LoggerContext = React.createContext<LoggerContextType>(
    {} as LoggerContextType
);



// ======================
// === LoggerProvider ===
// ======================

interface LoggerProviderProps {
    children: React.ReactNode;
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

export const useLogger = () => React.useContext(LoggerContext);
