/** @file A React provider containing the diagnostic logger. */
import type { PropsWithChildren } from 'react'
import { LoggerContext, type Logger } from './constants'

/** Props for a {@link LoggerProvider}. */
export interface LoggerProviderProps extends Readonly<PropsWithChildren> {
  readonly logger: Logger
}

/** A React provider containing the diagnostic logger. */
export function LoggerProvider(props: LoggerProviderProps) {
  const { children, logger } = props
  return <LoggerContext.Provider value={logger}>{children}</LoggerContext.Provider>
}
