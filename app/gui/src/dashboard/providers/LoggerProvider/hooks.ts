/** @file Hooks for `LoggerProvider`. */

import { useContext } from 'react'
import { LoggerContext } from './constants'

/** A React context hook exposing the diagnostic logger. */
export function useLogger() {
  return useContext(LoggerContext)
}
