/** @file This file provides the DialogStackProvider component and related functionality. */
import { createStore, useStore } from '#/utilities/zustand'
import {
  memo,
  startTransition,
  useContext,
  useEffect,
  useState,
  type PropsWithChildren,
} from 'react'
import invariant from 'tiny-invariant'
import { DialogStackContext, type DialogStackContextType, type DialogStackItem } from './constants'

/** DialogStackProvider is a React component that provides the dialog stack context to its children. */
export function DialogStackProvider(props: PropsWithChildren) {
  const { children } = props

  const [store] = useState(() =>
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
export const DialogStackRegistrar = memo(function DialogStackRegistrar(props: DialogStackItem) {
  const { id, type } = props

  const store = useContext(DialogStackContext)
  invariant(store, 'DialogStackRegistrar must be used within a DialogStackProvider')

  const { add, slice } = useStore(store, (state) => ({ add: state.add, slice: state.slice }))

  useEffect(() => {
    startTransition(() => {
      add({ id, type })
    })

    return () => {
      startTransition(() => {
        slice(id)
      })
    }
  }, [add, slice, id, type])

  return null
})

