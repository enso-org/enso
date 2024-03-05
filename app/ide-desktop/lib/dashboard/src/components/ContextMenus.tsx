/** @file A context menu. */
import * as React from 'react'

import * as detect from 'enso-common/src/detect'

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

  return hidden ? (
    <>{children}</>
  ) : (
    <Modal
      className="absolute size-full overflow-hidden bg-dim"
      onContextMenu={innerEvent => {
        innerEvent.preventDefault()
      }}
    >
      <div
        data-testid="context-menus"
        style={{ left: event.pageX, top: event.pageY }}
        className={`pointer-events-none sticky flex w-min items-start gap-context-menus ${
          detect.isOnMacOS()
            ? '-translate-x-context-menu-macos-half-x'
            : '-translate-x-context-menu-half-x'
        }`}
        onClick={clickEvent => {
          clickEvent.stopPropagation()
        }}
      >
        {children}
      </div>
    </Modal>
  )
}
