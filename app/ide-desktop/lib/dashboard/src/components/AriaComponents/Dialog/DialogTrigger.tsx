/**
 * @file
 *
 * A DialogTrigger opens a dialog when a trigger element is pressed.
 */
import * as React from 'react'

import * as reactAriaComponents from 'react-aria-components'

import * as modalProvider from '#/providers/ModalProvider'

import type * as types from './types'

const PLACEHOLDER = <div />

/**
 * A DialogTrigger opens a dialog when a trigger element is pressed.
 */
export function DialogTrigger(props: types.DialogTriggerProps) {
  const { children, onOpenChange, ...triggerProps } = props

  const { setModal, unsetModal } = modalProvider.useSetModal()

  const onOpenChangeInternal = React.useCallback(
    (isOpened: boolean) => {
      if (isOpened) {
        // we're using a placeholder here just to let the rest of the code know that the modal is open
        setModal(PLACEHOLDER)
      } else {
        unsetModal()
      }

      onOpenChange?.(isOpened)
    },
    [setModal, unsetModal, onOpenChange]
  )

  return (
    <reactAriaComponents.DialogTrigger
      children={children}
      onOpenChange={onOpenChangeInternal}
      {...triggerProps}
    />
  )
}
