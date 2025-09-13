/** @file A DialogTrigger opens a dialog when a trigger element is pressed. */
import * as React from 'react'

import * as aria from '#/components/aria'

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { createHideableComponent } from '@react-aria/collections'
import { useOverlayTriggerState } from 'react-stately'

/** Props passed to the render function of a {@link DialogTrigger}. */
export interface DialogTriggerRenderProps {
  readonly isOpen: boolean
  readonly close: () => void
  readonly open: () => void
}
/** Props for a {@link DialogTrigger}. */
export interface DialogTriggerProps {
  /** The trigger element. */
  readonly children: [
    React.ReactElement | ((props: DialogTriggerRenderProps) => React.ReactElement),
    React.ReactElement | ((props: DialogTriggerRenderProps) => React.ReactElement),
  ]
  readonly onOpen?: (() => void) | undefined
  readonly onClose?: (() => void) | undefined
  /** Whether the overlay is open by default (controlled). */
  readonly isOpen?: boolean | undefined
  /** Whether the overlay is open by default (uncontrolled). */
  readonly defaultOpen?: boolean | undefined
  /** Handler that is called when the overlay's open state changes. */
  readonly onOpenChange?: ((isOpen: boolean) => void) | undefined
}

/** A DialogTrigger opens a dialog when a trigger element is pressed. */
export const DialogTrigger = createHideableComponent(function DialogTrigger(
  props: DialogTriggerProps,
) {
  const { children, onOpenChange, onOpen = () => {}, onClose = () => {} } = props

  // @ts-expect-error Typescript requires to explicitly add `undefined` to the props
  // But we can't do that for library types
  const state = useOverlayTriggerState(props)

  const onOpenStableCallback = useEventCallback(onOpen)
  const onCloseStableCallback = useEventCallback(onClose)

  const onOpenChangeInternal = useEventCallback((opened: boolean) => {
    if (!opened) {
      onCloseStableCallback()
    } else {
      onOpenStableCallback()
    }

    state.setOpen(opened)
    onOpenChange?.(opened)
  })

  React.useEffect(() => {
    if (state.isOpen) {
      onOpenStableCallback()
    }
  }, [state.isOpen, onOpenStableCallback])

  const [trigger, dialog] = children

  const renderProps = {
    isOpen: state.isOpen,
    close: state.close.bind(state),
    open: state.open.bind(state),
  } satisfies DialogTriggerRenderProps

  return (
    <aria.DialogTrigger {...state} onOpenChange={onOpenChangeInternal}>
      {typeof trigger === 'function' ? trigger(renderProps) : trigger}

      {typeof dialog === 'function' ? dialog(renderProps) : dialog}
    </aria.DialogTrigger>
  )
})
