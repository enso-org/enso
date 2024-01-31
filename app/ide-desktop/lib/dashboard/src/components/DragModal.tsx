/** @file Modal for confirming delete of any type of asset. */
import * as React from 'react'

import * as modalProvider from '#/providers/ModalProvider'

import Modal from '#/components/Modal'

// =================
// === Constants ===
// =================

/** The default offset (up and to the right) of the drag element. */
const DEFAULT_OFFSET_PX = 16

// =================
// === DragModal ===
// =================

/** Props for a {@link DragModal}. */
export interface DragModalProps extends React.PropsWithChildren<JSX.IntrinsicElements['div']> {
  event: React.DragEvent
  doCleanup: () => void
  offsetPx?: number
  offsetXPx?: number
  offsetYPx?: number
}

/** A modal for confirming the deletion of an asset. */
export default function DragModal(props: DragModalProps) {
  const {
    event,
    offsetPx,
    offsetXPx = DEFAULT_OFFSET_PX,
    offsetYPx = DEFAULT_OFFSET_PX,
    children,
    style,
    className,
    doCleanup,
    ...passthrough
  } = props
  const { unsetModal } = modalProvider.useSetModal()
  const [left, setLeft] = React.useState(event.pageX - (offsetPx ?? offsetXPx))
  const [top, setTop] = React.useState(event.pageY - (offsetPx ?? offsetYPx))

  React.useEffect(() => {
    const onDrag = (moveEvent: MouseEvent) => {
      if (moveEvent.pageX !== 0 || moveEvent.pageY !== 0) {
        setLeft(moveEvent.pageX - (offsetPx ?? offsetXPx))
        setTop(moveEvent.pageY - (offsetPx ?? offsetYPx))
      }
    }
    const onDragEnd = () => {
      doCleanup()
      unsetModal()
    }
    document.addEventListener('drag', onDrag)
    document.addEventListener('dragend', onDragEnd)
    return () => {
      document.removeEventListener('drag', onDrag)
      document.removeEventListener('dragend', onDragEnd)
    }
    // `doCleanup` is a callback, not a dependency.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [
    /* should never change */ offsetPx,
    /* should never change */ offsetXPx,
    /* should never change */ offsetYPx,
    /* should never change */ unsetModal,
  ])

  return (
    <Modal className="absolute overflow-hidden pointer-events-none w-full h-full">
      <div
        {...passthrough}
        style={{ left, top, ...style }}
        className={`relative w-min ${className ?? ''}`}
      >
        {children}
      </div>
    </Modal>
  )
}
