/** @file This file provides the DialogStackProvider component and related functionality. */

import * as React from 'react'

import invariant from 'tiny-invariant'

import type { StoreApi } from '#/utilities/zustand'
import { createStore, useStore } from '#/utilities/zustand'
import { findLastIndex } from '@/util/data/array'

/** Returns only dialog items from the full overlay stack. */
function getDialogsStack(stack: DialogStackItem[]) {
  return stack.filter((stackItem) => ['dialog-fullscreen', 'dialog'].includes(stackItem.type))
}

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
            dialogsStack: getDialogsStack(nextStack),
          }
        })
      },
      slice: (currentId) => {
        set((state) => {
          const index = findLastIndex(state.stack, (item) => item.id === currentId)
          if (index == null) {
            // eslint-disable-next-line no-restricted-properties
            console.warn(`
              DialogStackProvider: sliceFromStack: currentId ${currentId} is not present in the stack. \
              This is no-op but it might be a sign of a bug in the application. \
              Usually, this means that the underlaying component was closed manually or the stack was not \
              updated properly.
          `)

            return state
          }

          const nextStack = [...state.stack.slice(0, index), ...state.stack.slice(index + 1)]

          return {
            stack: nextStack,
            dialogsStack: getDialogsStack(nextStack),
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

  const { add, slice } = useStore(store, (state) => ({ add: state.add, slice: state.slice }), {
    areEqual: 'shallow',
  })

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

/**
 * Hook that returns true if the given id is the latest item in the dialog stack.
 */
// eslint-disable-next-line react-refresh/only-export-components
export function useIsLatestDialogStackItem(id: string) {
  const store = React.useContext(DialogStackContext)
  invariant(store, 'useIsLatestDialogStackItem must be used within a DialogStackProvider')

  return useStore(store, (state) => state.stack.at(-1)?.id === id, { unsafeEnableTransition: true })
}
