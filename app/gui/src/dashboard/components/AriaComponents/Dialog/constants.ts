/** @file Constants for `Dialog`. */
import { createContext } from 'react'

/** The context value for a dialog. */
export interface DialogContextValue {
  readonly close: () => void
  readonly dialogId: string
}

/** The context for a dialog. */
export const DialogContext = createContext<DialogContextValue | null>(null)
