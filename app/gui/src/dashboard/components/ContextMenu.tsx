/** @file A context menu. */
import { Popover } from '#/components/Dialog'
import { unsetModal } from '#/providers/ModalProvider'
import { twMerge } from '#/utilities/tailwindMerge'
import { isOnMacOS } from 'enso-common/src/detect'
import type { MouseEvent, PropsWithChildren } from 'react'

/** Props for a `ContextMenu`. */
export interface ContextMenuProps extends Readonly<PropsWithChildren> {
  // eslint-disable-next-line @typescript-eslint/naming-convention
  readonly 'aria-label': string
  readonly hidden?: boolean
  readonly event: Pick<MouseEvent, 'pageX' | 'pageY'>
}

/** A context menu that opens at the current mouse position. */
export default function ContextMenu(props: ContextMenuProps) {
  const { hidden = false, children, event } = props

  if (hidden) {
    return children
  }

  return (
    <Popover
      data-testid="context-menu"
      defaultOpen
      style={{ left: event.pageX, top: event.pageY }}
      shouldCloseOnInteractOutside={() => true}
      className="sticky flex w-min items-start"
      onClose={unsetModal}
    >
      <div
        aria-label={props['aria-label']}
        className={twMerge(
          'relative flex flex-col rounded-default',
          isOnMacOS() ? 'w-context-menu-macos' : 'w-context-menu',
        )}
      >
        {children}
      </div>
    </Popover>
  )
}
