/** @file A context menu. */
import * as React from 'react'

import * as detect from 'enso-common/src/detect'

import FocusArea from '#/components/styled/FocusArea'

import * as tailwindMerge from '#/utilities/tailwindMerge'

// ===================
// === ContextMenu ===
// ===================

/** Props for a {@link ContextMenu}. */
export interface ContextMenuProps extends Readonly<React.PropsWithChildren> {
  // eslint-disable-next-line @typescript-eslint/naming-convention
  readonly 'aria-label': string
  readonly hidden?: boolean
}

/** A context menu that opens at the current mouse position. */
export default function ContextMenu(props: ContextMenuProps) {
  const { hidden = false, children } = props

  return hidden ? children : (
      <FocusArea direction="vertical">
        {(innerProps) => (
          <div
            className="pointer-events-auto relative rounded-default before:absolute before:h-full before:w-full before:rounded-default before:bg-selected-frame before:backdrop-blur-default"
            {...innerProps}
          >
            <div
              aria-label={props['aria-label']}
              className={tailwindMerge.twMerge(
                'relative flex flex-col rounded-default p-context-menu',
                detect.isOnMacOS() ? 'w-context-menu-macos' : 'w-context-menu',
              )}
            >
              {children}
            </div>
          </div>
        )}
      </FocusArea>
    )
}
