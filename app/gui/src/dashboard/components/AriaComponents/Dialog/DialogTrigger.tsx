/** @file A DialogTrigger opens a dialog when a trigger element is pressed. */
import * as React from 'react'

import * as modalProvider from '#/providers/ModalProvider'

import * as aria from '#/components/aria'

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useOverlayTriggerState } from 'react-stately'

const PLACEHOLDER = <div />

/** Props passed to the render function of a {@link DialogTrigger}. */
export interface DialogTriggerRenderProps {
  readonly isOpen: boolean
  readonly close: () => void
  readonly open: () => void
}
/** Props for a {@link DialogTrigger}. */
export interface DialogTriggerProps extends Omit<aria.DialogTriggerProps, 'children'> {
  /** The trigger element. */
  readonly children: [
    React.ReactElement,
    React.ReactElement | ((props: DialogTriggerRenderProps) => React.ReactElement),
  ]
  readonly onOpen?: () => void
  readonly onClose?: () => void
}

/** A DialogTrigger opens a dialog when a trigger element is pressed. */
export function DialogTrigger(props: DialogTriggerProps) {
  const { children, onOpenChange, onOpen = () => {}, onClose = () => {} } = props

  const state = useOverlayTriggerState(props)

  const { setModal, unsetModal } = modalProvider.useSetModal()

  const onOpenStableCallback = useEventCallback(onOpen)
  const onCloseStableCallback = useEventCallback(onClose)

  const onOpenChangeInternal = useEventCallback((opened: boolean) => {
    if (opened) {
      // We're using a placeholder here just to let the rest of the code know that the modal
      // is open.
      setModal(PLACEHOLDER)
    } else {
      unsetModal()
      onCloseStableCallback()
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
      {trigger}

      {typeof dialog === 'function' ? dialog(renderProps) : dialog}
    </aria.DialogTrigger>
  )
}
