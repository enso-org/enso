/** @file Hooks for `Dialog`. */
import { useContext } from 'react'
import invariant from 'tiny-invariant'
import { DialogContext } from './constants'

/** Custom hook to get the dialog context. */
export function useDialogContext() {
  return useContext(DialogContext)
}

/**
 * Custom hook to get the dialog context.
 * @throws if the hook is used outside of a DialogProvider
 */
export function useDialogStrictContext() {
  const context = useDialogContext()
  invariant(context != null, 'useDialogStrictContext must be used within a DialogProvider')
  return context
}
