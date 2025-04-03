/** @file A context menu. */
import { FocusArea } from '#/components/styled/FocusArea'
import { forwardRef } from '#/utilities/react'
import { twMerge } from '#/utilities/tailwindMerge'
import { isOnMacOS } from 'enso-common/src/detect'
import type { ForwardedRef, MouseEvent, PropsWithChildren } from 'react'
import { Modal } from './Modal'

/** Props for a `ContextMenu`. */
export interface ContextMenuProps extends Readonly<PropsWithChildren> {
  // eslint-disable-next-line @typescript-eslint/naming-convention
  readonly 'aria-label': string
  readonly hidden?: boolean
  readonly event: Pick<MouseEvent, 'pageX' | 'pageY'>
}

/** A context menu that opens at the current mouse position. */
export const ContextMenu = forwardRef(function ContextMenu(
  props: ContextMenuProps,
  ref: ForwardedRef<HTMLDivElement>,
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
          className={twMerge('pointer-events-none sticky flex w-min items-start gap-context-menus')}
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
                  className={twMerge(
                    'relative flex flex-col rounded-default p-context-menu',
                    isOnMacOS() ? 'w-context-menu-macos' : 'w-context-menu',
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
