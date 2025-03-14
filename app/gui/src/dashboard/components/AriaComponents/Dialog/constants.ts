/** @file Constants for `Dialog`. */
import { createContext } from 'react'
import type { StoreApi } from 'zustand'

/** The context value for a dialog. */
export interface DialogContextValue {
  readonly close: () => void
  readonly dialogId: string
}

/** The context for a dialog. */
export const DialogContext = createContext<DialogContextValue | null>(null)

/** DialogStackItem represents an item in the dialog stack. */
export interface DialogStackItem {
  readonly id: string
  readonly type: 'dialog-fullscreen' | 'dialog' | 'popover'
}

/** DialogStackContextType represents the context for the dialog stack. */
export interface DialogStackContextType {
  readonly stack: DialogStackItem[]
  readonly dialogsStack: DialogStackItem[]
  readonly add: (item: DialogStackItem) => void
  readonly slice: (currentId: string) => void
}

export const DialogStackContext = createContext<StoreApi<DialogStackContextType> | null>(null)
