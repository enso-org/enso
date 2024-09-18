/** @file A component that renders the modal instance from the modal React Context. */
import * as React from 'react'

import { DialogTrigger } from '#/components/AriaComponents'
import * as modalProvider from '#/providers/ModalProvider'
import { AnimatePresence, motion } from 'framer-motion'

// ================
// === TheModal ===
// ================

/** Renders the modal instance from the modal React Context (if any). */
export default function TheModal() {
  const { modal, key } = modalProvider.useModal()

  return (
    <AnimatePresence>
      {modal && (
        <motion.div
          initial={{ opacity: 1 }}
          animate={{ opacity: 1 }}
          /* eslint-disable-next-line @typescript-eslint/no-magic-numbers */
          exit={{ opacity: 0.8 }}
          /* eslint-disable-next-line @typescript-eslint/no-magic-numbers */
          transition={{ duration: 0.2 }}
        >
          <DialogTrigger key={key} defaultOpen>
            <></>
            {modal}
          </DialogTrigger>
        </motion.div>
      )}
    </AnimatePresence>
  )
}
