/** @file A DialogTrigger opens a dialog when a trigger element is pressed. */
import * as React from 'react'

import * as modalProvider from '#/providers/ModalProvider'

import * as aria from '#/components/aria'

import { AnimatePresence, motion } from 'framer-motion'

const PLACEHOLDER = <div />

/**
 * Props passed to the render function of a {@link DialogTrigger}.
 */
export interface DialogTriggerRenderProps {
  readonly isOpened: boolean
}
/**
 * Props for a {@link DialogTrigger}.
 */
export interface DialogTriggerProps extends Omit<aria.DialogTriggerProps, 'children'> {
  /**
   * The trigger element.
   */
  readonly children: [
    React.ReactElement,
    React.ReactElement | ((props: DialogTriggerRenderProps) => React.ReactElement),
  ]
}

/** A DialogTrigger opens a dialog when a trigger element is pressed. */
export function DialogTrigger(props: DialogTriggerProps) {
  const { children, onOpenChange, ...triggerProps } = props

  const [isOpened, setIsOpened] = React.useState(false)
  const { setModal, unsetModal } = modalProvider.useSetModal()

  const onOpenChangeInternal = React.useCallback(
    (opened: boolean) => {
      if (opened) {
        // We're using a placeholder here just to let the rest of the code know that the modal
        // is open.
        setModal(PLACEHOLDER)
      } else {
        unsetModal()
      }

      setIsOpened(opened)
      onOpenChange?.(opened)
    },
    [setModal, unsetModal, onOpenChange],
  )

  const renderProps = {
    isOpened,
  } satisfies DialogTriggerRenderProps

  const [trigger, dialog] = children

  return (
    <aria.DialogTrigger onOpenChange={onOpenChangeInternal} {...triggerProps}>
      {trigger}

      {/* We're using AnimatePresence here to animate the dialog in and out. */}
      <AnimatePresence>
        {isOpened && (
          <motion.div
            style={{ display: 'none' }}
            initial={{ opacity: 1 }}
            animate={{ opacity: 1 }}
            exit={{ opacity: 0 }}
            transition={{ duration: 1 }}
          >
            {typeof dialog === 'function' ? dialog(renderProps) : dialog}
          </motion.div>
        )}
      </AnimatePresence>
    </aria.DialogTrigger>
  )
}
