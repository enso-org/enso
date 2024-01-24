/** @file A context menu. */
import * as React from 'react'

import * as detect from 'enso-common/src/detect'

// ===================
// === ContextMenu ===
// ===================

/** Props for a {@link ContextMenu}. */
export interface ContextMenuProps extends React.PropsWithChildren {
  hidden?: boolean
}

/** A context menu that opens at the current mouse position. */
export default function ContextMenu(props: ContextMenuProps) {
  const { hidden = false, children } = props

  return hidden ? (
    <>{children}</>
  ) : (
    <div className="relative rounded-2xl pointer-events-auto before:absolute before:rounded-2xl before:bg-frame-selected before:backdrop-blur-3xl before:w-full before:h-full">
      <div
        className={`relative flex flex-col rounded-2xl ${
          detect.isOnMacOS() ? 'w-57.5' : 'w-62'
        } p-2`}
        onClick={clickEvent => {
          clickEvent.stopPropagation()
        }}
      >
        {children}
      </div>
    </div>
  )
}
