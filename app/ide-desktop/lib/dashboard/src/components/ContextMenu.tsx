/** @file A context menu. */
import * as React from 'react'

import * as detect from 'enso-common/src/detect'

// ===================
// === ContextMenu ===
// ===================

/** Props for a {@link ContextMenu}. */
export interface ContextMenuProps extends Readonly<React.PropsWithChildren> {
  readonly hidden?: boolean
}

/** A context menu that opens at the current mouse position. */
export default function ContextMenu(props: ContextMenuProps) {
  const { hidden = false, children } = props

  return hidden ? (
    <>{children}</>
  ) : (
    <div className="relative rounded-default pointer-events-auto before:absolute before:rounded-default before:bg-selected-frame before:backdrop-blur-default before:w-full before:h-full">
      <div
        className={`relative flex flex-col rounded-default ${
          detect.isOnMacOS() ? 'w-context-menu-macos' : 'w-context-menu'
        } p-context-menu`}
        onClick={clickEvent => {
          clickEvent.stopPropagation()
        }}
      >
        {children}
      </div>
    </div>
  )
}
