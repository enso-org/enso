/** @file A component that renders the modal instance from the modal React Context. */
import { Pressable } from '#/components/aria'
import { Dialog } from '#/components/Dialog'
import { useModal } from '#/providers/ModalProvider'
import { AnimatePresence, motion } from 'framer-motion'

/** Renders the modal instance from the modal React Context (if any). */
export default function TheModal() {
  const { modal, key } = useModal()

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
