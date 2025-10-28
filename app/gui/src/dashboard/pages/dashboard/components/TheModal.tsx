/** @file A component that renders the modal instance from the modal React Context. */
import { Pressable } from '#/components/aria'
import { Dialog } from '#/components/Dialog'
import { useModal } from '#/providers/ModalProvider'

/** Renders the modal instance from the modal React Context (if any). */
export default function TheModal() {
  const { modal, key } = useModal()

  return (
    modal && (
      <Dialog.Trigger key={key} defaultOpen>
        {/* This component suppresses the warning about the target not being pressable element. */}
        <Pressable>
          <></>
        </Pressable>

        {modal}
      </Dialog.Trigger>
    )
  )
}
