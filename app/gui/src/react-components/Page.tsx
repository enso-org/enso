/** @file A page. */
import { useModal } from '#/providers/ModalProvider'
import { Pressable } from '$/react-components/aria'
import { Dialog } from '$/react-components/Dialog'
import Portal from '$/react-components/Portal'
import InfoBar from '$/user-bar/InfoBar'
import { AnimatePresence, motion } from 'framer-motion'
import * as React from 'react'

/** Props for a {@link Page}. */
export interface PageProps extends Readonly<React.PropsWithChildren> {
  readonly hideInfoBar?: true
}

/** A page. */
export default function Page(props: PageProps) {
  const { hideInfoBar = false, children } = props

  return (
    <>
      {children}
      {!hideInfoBar && (
        <div className="fixed right top z-1 m-2.5 text-primary">
          <InfoBar />
        </div>
      )}
      <Portal>
        <div className="select-none text-xs text-primary">
          <TheModal />
        </div>
      </Portal>
    </>
  )
}

/** Renders the modal instance from the modal React Context (if any). */
function TheModal() {
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
