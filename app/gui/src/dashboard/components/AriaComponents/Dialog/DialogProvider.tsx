/**
 * @file
 *
 * The context value for a dialog.
 */
import * as React from 'react'

/** The context value for a dialog. */
export interface DialogContextValue {
  readonly close: () => void
  readonly dialogId: string
}

/** The context for a dialog. */
const DialogContext = React.createContext<DialogContextValue | null>(null)

/** The provider for a dialog. */
// eslint-disable-next-line no-restricted-syntax
export const DialogProvider = DialogContext.Provider

/** Custom hook to get the dialog context. */
export function useDialogContext() {
  return React.useContext(DialogContext)
}
