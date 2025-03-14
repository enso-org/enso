/** @file Hooks for `UIProviders`. */
import { useContext } from 'react'
import { RootContext } from './constants'

/** A hook to get the root elements of the application. */
export function useRootContext() {
  return useContext(RootContext)
}
