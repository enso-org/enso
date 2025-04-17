/**
 * @file
 *
 * Provides the {@link AlertContext} to the {@link AlertDialog} component.
 */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { createContext, useContext } from 'react'
import invariant from 'tiny-invariant'
import { useDialogStrictContext } from '../Dialog'

/**
 * The context value for the {@link AlertDialogProvider}.
 */
interface AlertContextValue {
  readonly dismiss: () => Promise<void> | void
  readonly confirm: () => Promise<void> | void
  readonly isDestructive: boolean
}

const AlertContext = createContext<AlertContextValue | null>(null)

/**
 * Props for the {@link AlertDialogProvider} component.
 */
export interface AlertDialogProviderProps extends React.PropsWithChildren {
  readonly dismiss?: (() => Promise<void> | void) | null | undefined
  readonly confirm?: (() => Promise<void> | void) | null | undefined
  readonly isDestructive?: boolean
}

/**
 * Props for the {@link AlertDialogProvider} component.
 */
export function AlertDialogProvider(props: AlertDialogProviderProps) {
  const { dismiss: dismissProp, confirm: confirmProp, children, isDestructive = false } = props

  const dialogContext = useDialogStrictContext()

  const dismiss = useEventCallback(() => {
    dialogContext.close()
    return dismissProp?.()
  })

  const confirm = useEventCallback(() => {
    dialogContext.close()
    return confirmProp?.()
  })

  return (
    <AlertContext.Provider value={{ dismiss, confirm, isDestructive }}>
      {children}
    </AlertContext.Provider>
  )
}

/**
 * Custom hook to get the {@link AlertContext} context.
 * @throws if the hook is used outside of a {@link AlertDialogProvider}.
 */
AlertDialogProvider.useContextStrict = () => {
  const context = useContext(AlertContext)

  invariant(context != null, 'useAlertDialogContext must be used within an AlertDialogProvider')
  return context
}

/**
 * Custom hook to get the {@link AlertContext} context.
 */
AlertDialogProvider.useContext = () => {
  return useContext(AlertContext)
}
