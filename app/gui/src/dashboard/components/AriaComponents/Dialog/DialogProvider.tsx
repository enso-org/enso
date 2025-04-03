/**
 * @file
 *
 * The context value for a dialog.
 */
import * as React from 'react'
import invariant from 'tiny-invariant'

/** The context value for a dialog. */
export interface DialogContextValue {
  readonly close: () => void
  readonly dialogId: string
}

/** The context for a dialog. */
const DialogContext = React.createContext<DialogContextValue | null>(null)

/** The provider for a dialog. */
export function DialogProvider(props: DialogContextValue & React.PropsWithChildren) {
  const { children, close, dialogId } = props

  const value = React.useMemo(() => ({ close, dialogId }), [close, dialogId])

  return <DialogContext.Provider value={value}>{children}</DialogContext.Provider>
}

/** Custom hook to get the dialog context. */
export function useDialogContext() {
  return React.useContext(DialogContext)
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
