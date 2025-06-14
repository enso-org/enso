/** @file A component that renders the modal instance from the modal React Context. */
import * as modalProvider from '#/providers/ModalProvider'
import { Pressable } from '$/react-components/aria'
import { Dialog } from '$/react-components/Dialog'
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
            {/* This component suppresses the warning about the target not being pressable element. */}
            <Pressable>
              <></>
            </Pressable>

            {modal}
          </Dialog.Trigger>
        </motion.div>
      )}
    </AnimatePresence>
  )
}
