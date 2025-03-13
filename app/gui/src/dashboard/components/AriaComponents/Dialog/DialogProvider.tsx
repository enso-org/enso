/** @file React context provider for a dialog. */
import { useMemo, type PropsWithChildren } from 'react'
import { DialogContext, type DialogContextValue } from './constants'

/** The provider for a dialog. */
export function DialogProvider(props: DialogContextValue & PropsWithChildren) {
  const { children, close, dialogId } = props

  const value = useMemo(() => ({ close, dialogId }), [close, dialogId])

  return <DialogContext.Provider value={value}>{children}</DialogContext.Provider>
}
