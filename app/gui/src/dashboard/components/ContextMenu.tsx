/** @file A context menu. */
import * as React from 'react'

import * as detect from 'enso-common/src/detect'

import FocusArea from '#/components/styled/FocusArea'

import { forwardRef } from '#/utilities/react'
import * as tailwindMerge from '#/utilities/tailwindMerge'
import Modal from './Modal'

/** Props for a `ContextMenu`. */
export interface ContextMenuProps extends Readonly<React.PropsWithChildren> {
  // eslint-disable-next-line @typescript-eslint/naming-convention
  readonly 'aria-label': string
  readonly hidden?: boolean
  readonly event: Pick<React.MouseEvent, 'pageX' | 'pageY'>
}

/** A context menu that opens at the current mouse position. */
export default forwardRef(function ContextMenu(
  props: ContextMenuProps,
  ref: React.ForwardedRef<HTMLDivElement>,
) {
  const { hidden = false, children, event } = props

  return hidden ? children : (
      <Modal
        className="absolute size-full overflow-hidden bg-dim"
        onContextMenu={(innerEvent) => {
          innerEvent.preventDefault()
        }}
      >
        <div
          data-testid="context-menu"
          ref={ref}
          style={{ left: event.pageX, top: event.pageY }}
          className={tailwindMerge.twMerge(
            'pointer-events-none sticky flex w-min items-start gap-context-menus',
          )}
          onClick={(clickEvent) => {
            clickEvent.stopPropagation()
          }}
        >
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
        </div>
      </Modal>
    )
})
