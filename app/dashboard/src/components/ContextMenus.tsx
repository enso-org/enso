/** @file A context menu. */
import * as React from 'react'

import * as detect from 'enso-common/src/detect'

import Modal from '#/components/Modal'

import { forwardRef } from '#/utilities/react'
import * as tailwindMerge from '#/utilities/tailwindMerge'

// =================
// === Constants ===
// =================

const DEFAULT_MENU_WIDTH = 256
const MACOS_MENU_WIDTH = 230
/** The width of a single context menu. */
const MENU_WIDTH = detect.isOnMacOS() ? MACOS_MENU_WIDTH : DEFAULT_MENU_WIDTH
const HALF_MENU_WIDTH = Math.floor(MENU_WIDTH / 2)

// ===================
// === ContextMenu ===
// ===================

/** Props for a {@link ContextMenus}. */
export interface ContextMenusProps extends Readonly<React.PropsWithChildren> {
  readonly hidden?: boolean
  readonly key: string
  readonly event: Pick<React.MouseEvent, 'pageX' | 'pageY'>
}

export default forwardRef(ContextMenus)

/** A context menu that opens at the current mouse position. */
function ContextMenus(props: ContextMenusProps, ref: React.ForwardedRef<HTMLDivElement>) {
  const { hidden = false, children, event } = props

  return hidden ?
      <>{children}</>
    : <Modal
        className="absolute size-full overflow-hidden bg-dim"
        onContextMenu={(innerEvent) => {
          innerEvent.preventDefault()
        }}
      >
        <div
          data-testid="context-menus"
          ref={ref}
          style={{ left: event.pageX - HALF_MENU_WIDTH, top: event.pageY }}
          className={tailwindMerge.twMerge(
            'pointer-events-none sticky flex w-min items-start gap-context-menus',
          )}
          onClick={(clickEvent) => {
            clickEvent.stopPropagation()
          }}
        >
          {children}
        </div>
      </Modal>
}
