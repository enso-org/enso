/** @file A component that renders the modal instance from the modal React Context. */
import { usePress } from '#/components/aria'
import { Dialog } from '#/components/Dialog'
import * as modalProvider from '#/providers/ModalProvider'
import { AnimatePresence, motion } from 'framer-motion'

/** Renders the modal instance from the modal React Context (if any). */
export default function TheModal() {
  const { modal, key } = modalProvider.useModal()

  return (
    <AnimatePresence>
      {modal && (
        <motion.div
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          exit={{ opacity: 0 }}
          // eslint-disable-next-line @typescript-eslint/no-magic-numbers
          transition={{ duration: 0.2 }}
        >
          <Dialog.Trigger key={key} defaultOpen>
            <MockPressable />

            {modal}
          </Dialog.Trigger>
        </motion.div>
      )}
    </AnimatePresence>
  )
}

/**
 * A mock `Pressable` that does nothing.
 * This is used as the trigger for the `Dialog.Trigger` above to suppress any warning logs.
 */
function MockPressable() {
  const { pressProps } = usePress({})

  return <div {...pressProps} />
}
