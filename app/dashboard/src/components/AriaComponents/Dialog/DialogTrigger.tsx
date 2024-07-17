/** @file A DialogTrigger opens a dialog when a trigger element is pressed. */
import * as React from 'react'

import * as modalProvider from '#/providers/ModalProvider'

import * as aria from '#/components/aria'

import type * as types from './types'

const PLACEHOLDER = <div />

/**
 * Props for a {@link DialogTrigger}.
 */
export interface DialogTriggerProps extends types.DialogTriggerProps {}

/** A DialogTrigger opens a dialog when a trigger element is pressed. */
export function DialogTrigger(props: DialogTriggerProps) {
  const { children, onOpenChange, ...triggerProps } = props

  const { setModal, unsetModal } = modalProvider.useSetModal()

  const onOpenChangeInternal = React.useCallback(
    (isOpened: boolean) => {
      if (isOpened) {
        // We're using a placeholder here just to let the rest of the code know that the modal
        // is open.
        setModal(PLACEHOLDER)
      } else {
        unsetModal()
      }

      onOpenChange?.(isOpened)
    },
    [setModal, unsetModal, onOpenChange]
  )

  return (
    <aria.DialogTrigger onOpenChange={onOpenChangeInternal} {...triggerProps}>
      {children}
    </aria.DialogTrigger>
  )
}
