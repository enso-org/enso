/** @file This file provides the DialogStackProvider component and related functionality. */

import * as React from 'react'

import invariant from 'tiny-invariant'

import type { StoreApi } from '#/utilities/zustand'
import { createStore, useStore } from '#/utilities/zustand'

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

const DialogStackContext = React.createContext<StoreApi<DialogStackContextType> | null>(null)

/** DialogStackProvider is a React component that provides the dialog stack context to its children. */
export function DialogStackProvider(props: React.PropsWithChildren) {
  const { children } = props

  const [store] = React.useState(() =>
    createStore<DialogStackContextType>((set) => ({
      stack: [],
      dialogsStack: [],
      add: (item) => {
        set((state) => {
          const nextStack = [...state.stack, item]

          return {
            stack: nextStack,
            dialogsStack: nextStack.filter((stackItem) =>
              ['dialog-fullscreen', 'dialog'].includes(stackItem.type),
            ),
          }
        })
      },
      slice: (currentId) => {
        set((state) => {
          const lastItem = state.stack.at(-1)
          if (lastItem?.id === currentId) {
            return { stack: state.stack.slice(0, -1) }
          } else {
            // eslint-disable-next-line no-restricted-properties
            console.warn(`
              DialogStackProvider: sliceFromStack: currentId ${currentId} does not match the last item in the stack. \
              This is no-op but it might be a sign of a bug in the application. \
              Usually, this means that the underlaying component was closed manually or the stack was not \
              updated properly.
          `)

            return { stack: state.stack }
          }
        })
      },
    })),
  )

  return <DialogStackContext.Provider value={store}>{children}</DialogStackContext.Provider>
}

/** DialogStackRegistrar is a React component that registers a dialog in the dialog stack. */
export const DialogStackRegistrar = React.memo(function DialogStackRegistrar(
  props: DialogStackItem,
) {
  const { id, type } = props

  const store = React.useContext(DialogStackContext)
  invariant(store, 'DialogStackRegistrar must be used within a DialogStackProvider')

  const { add, slice } = useStore(store, (state) => ({ add: state.add, slice: state.slice }))

  React.useEffect(() => {
    React.startTransition(() => {
      add({ id, type })
    })

    return () => {
      React.startTransition(() => {
        slice(id)
      })
    }
  }, [add, slice, id, type])

  return null
})

/** Props for {@link useDialogStackState} */
export interface UseDialogStackStateProps {
  readonly id: string
}

/** useDialogStackState is a custom hook that provides the state of the dialog stack. */
export function useDialogStackState(props: UseDialogStackStateProps) {
  const store = React.useContext(DialogStackContext)
  invariant(store, 'useDialogStackState must be used within a DialogStackProvider')

  const isLatest = useIsLatestDialogStackItem(props.id)
  const index = useDialogStackIndex(props.id)

  return { isLatest, index }
}

/**
 * Hook that returns true if the given id is the latest item in the dialog stack.
 */
export function useIsLatestDialogStackItem(id: string) {
  const store = React.useContext(DialogStackContext)
  invariant(store, 'useIsLatestDialogStackItem must be used within a DialogStackProvider')

  return useStore(store, (state) => state.stack.at(-1)?.id === id, { unsafeEnableTransition: true })
}

/**
 * Hook that returns the index of the given id in the dialog stack.
 */
export function useDialogStackIndex(id: string) {
  const store = React.useContext(DialogStackContext)
  invariant(store, 'useDialogStackIndex must be used within a DialogStackProvider')

  return useStore(store, (state) => state.stack.findIndex((item) => item.id === id), {
    unsafeEnableTransition: true,
  })
}
