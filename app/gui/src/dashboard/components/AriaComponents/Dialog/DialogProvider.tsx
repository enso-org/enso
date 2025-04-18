/** @file React context provider for a dialog. */
import type { PropsWithChildren } from 'react'
import { DialogContext, type DialogContextValue } from './constants'

/** The provider for a dialog. */
export function DialogProvider(props: DialogContextValue & PropsWithChildren) {
  const { children, close, dialogId } = props

  return <DialogContext.Provider value={{ close, dialogId }}>{children}</DialogContext.Provider>
}
