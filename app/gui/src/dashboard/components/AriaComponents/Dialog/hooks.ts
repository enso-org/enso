/** @file Hooks for `Dialog`. */
import { useStore } from '#/hooks/storeHooks'
import { useContext } from 'react'
import invariant from 'tiny-invariant'
import { DialogContext, DialogStackContext } from './constants'

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

/** Options for {@link useDialogStackState} */
export interface UseDialogStackStateOptions {
  readonly id: string
}

/** useDialogStackState is a custom hook that provides the state of the dialog stack. */
export function useDialogStackState(props: UseDialogStackStateOptions) {
  const store = useContext(DialogStackContext)
  invariant(store, 'useDialogStackState must be used within a DialogStackProvider')

  const isLatest = useIsLatestDialogStackItem(props.id)
  const index = useDialogStackIndex(props.id)

  return { isLatest, index }
}

/** Hook that returns true if the given id is the latest item in the dialog stack. */
export function useIsLatestDialogStackItem(id: string) {
  const store = useContext(DialogStackContext)
  invariant(store, 'useIsLatestDialogStackItem must be used within a DialogStackProvider')

  return useStore(store, (state) => state.stack.at(-1)?.id === id, { unsafeEnableTransition: true })
}

/** Hook that returns the index of the given id in the dialog stack. */
export function useDialogStackIndex(id: string) {
  const store = useContext(DialogStackContext)
  invariant(store, 'useDialogStackIndex must be used within a DialogStackProvider')

  return useStore(store, (state) => state.stack.findIndex((item) => item.id === id), {
    unsafeEnableTransition: true,
  })
}
