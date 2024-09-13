/** @file A DialogTrigger opens a dialog when a trigger element is pressed. */
import * as React from 'react'

import * as modalProvider from '#/providers/ModalProvider'

import * as aria from '#/components/aria'

import { useEventCallback } from '#/hooks/eventCallbackHooks'
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
  readonly onOpen?: () => void
  readonly onClose?: () => void
}

/** A DialogTrigger opens a dialog when a trigger element is pressed. */
export function DialogTrigger(props: DialogTriggerProps) {
  const { children, onOpenChange, onOpen = () => {}, onClose = () => {}, ...triggerProps } = props

  const [isOpened, setIsOpened] = React.useState(
    triggerProps.isOpen ?? triggerProps.defaultOpen ?? false,
  )
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

    setIsOpened(opened)
    onOpenChange?.(opened)
  })

  React.useEffect(() => {
    if (isOpened) {
      onOpenStableCallback()
    }
  }, [isOpened, onOpenStableCallback])

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
