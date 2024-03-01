/** @file A context menu. */
import * as React from 'react'

import Modal from '#/components/Modal'

// ===================
// === ContextMenu ===
// ===================

/** Props for a {@link ContextMenus}. */
export interface ContextMenusProps extends Readonly<React.PropsWithChildren> {
  readonly hidden?: boolean
  readonly key: string
  readonly event: Pick<React.MouseEvent, 'pageX' | 'pageY'>
}

/** A context menu that opens at the current mouse position. */
export default function ContextMenus(props: ContextMenusProps) {
  const { hidden = false, children, event } = props
  const contextMenuRef = React.useRef<HTMLDivElement>(null)
  const [left, setLeft] = React.useState(event.pageX)
  const [top, setTop] = React.useState(event.pageY)

  React.useLayoutEffect(() => {
    if (contextMenuRef.current != null) {
      setTop(Math.min(top, window.innerHeight - contextMenuRef.current.clientHeight))
      const boundingBox = contextMenuRef.current.getBoundingClientRect()
      setLeft(event.pageX - boundingBox.width / 2)
    }
  }, [children, top, event.pageX])

  return hidden ? (
    <>{children}</>
  ) : (
    <Modal
      className="absolute overflow-hidden bg-dim size-full"
      onContextMenu={innerEvent => {
        innerEvent.preventDefault()
      }}
    >
      <div
        data-testid="context-menus"
        ref={contextMenuRef}
        style={{ left, top }}
        className="sticky flex pointer-events-none items-start gap-context-menus w-min"
        onClick={clickEvent => {
          clickEvent.stopPropagation()
        }}
      >
        {children}
      </div>
    </Modal>
  )
}
