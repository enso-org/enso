/** @file This file provides the DialogStackProvider component and related functionality. */

import * as React from 'react'

import invariant from 'tiny-invariant'

import * as eventCallbackHooks from '#/hooks/eventCallbackHooks'

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

const DialogStackContext = React.createContext<DialogStackContextType | null>(null)

/** DialogStackProvider is a React component that provides the dialog stack context to its children. */
export function DialogStackProvider(props: React.PropsWithChildren) {
  const { children } = props

  const [stack, setStack] = React.useState<DialogStackItem[]>([])

  const addToStack = eventCallbackHooks.useEventCallback((item: DialogStackItem) => {
    setStack((currentStack) => [...currentStack, item])
  })

  const sliceFromStack = eventCallbackHooks.useEventCallback((currentId: string) => {
    setStack((currentStack) => {
      const lastItem = currentStack.at(-1)

      if (lastItem?.id === currentId) {
        return currentStack.slice(0, -1)
      } else {
        // eslint-disable-next-line no-restricted-properties
        console.warn(`
DialogStackProvider: sliceFromStack: currentId ${currentId} does not match the last item in the stack. \
This is no-op but it might be a sign of a bug in the application. \
Usually, this means that the underlaying component was closed manually or the stack was not \
updated properly.`)

        return currentStack
      }
    })
  })

  const value = React.useMemo<DialogStackContextType>(
    () => ({
      stack,
      dialogsStack: stack.filter((item) => ['dialog-fullscreen', 'dialog'].includes(item.type)),
      add: addToStack,
      slice: sliceFromStack,
    }),
    [stack, addToStack, sliceFromStack],
  )

  return <DialogStackContext.Provider value={value}>{children}</DialogStackContext.Provider>
}

/** DialogStackRegistrar is a React component that registers a dialog in the dialog stack. */
export function DialogStackRegistrar(props: React.PropsWithChildren<DialogStackItem>) {
  const { children, id: idRaw, type: typeRaw } = props
  const idRef = React.useRef(idRaw)
  const typeRef = React.useRef(typeRaw)

  const context = React.useContext(DialogStackContext)

  invariant(context, 'DialogStackRegistrar must be used within a DialogStackProvider')

  const { add, slice } = context

  React.useEffect(() => {
    const id = idRef.current
    const type = typeRef.current

    add({ id, type })

    return () => {
      slice(id)
    }
  }, [add, slice])

  return children
}

/** Props for {@link useDialogStackState} */
export interface UseDialogStackStateProps {
  readonly id: string
}

/** useDialogStackState is a custom hook that provides the state of the dialog stack. */
export function useDialogStackState(props: UseDialogStackStateProps) {
  const ctx = React.useContext(DialogStackContext)

  invariant(ctx, 'useDialogStackState must be used within a DialogStackProvider')

  const { id } = props

  const isLatest = ctx.stack.at(-1)?.id === id
  const index = ctx.stack.findIndex((item) => item.id === id)

  return { isLatest, index }
}
