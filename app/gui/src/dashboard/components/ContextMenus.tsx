/** @file A context menu. */
import * as React from 'react'

import Modal from '#/components/Modal'

import { forwardRef } from '#/utilities/react'
import * as tailwindMerge from '#/utilities/tailwindMerge'

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
          style={{ left: event.pageX, top: event.pageY }}
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
