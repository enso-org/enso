/** @file A context menu. */
import { Pressable } from '#/components/aria'
import { Popover } from '#/components/Dialog'
import { usePortalContext } from '#/components/Portal'
import { unsetModal } from '#/providers/ModalProvider'
import { twMerge } from '#/utilities/tailwindMerge'
import { isOnMacOS } from 'enso-common/src/detect'
import {
  forwardRef,
  useImperativeHandle,
  useState,
  type ForwardedRef,
  type MouseEvent,
  type PropsWithChildren,
} from 'react'

/** Props for a {@link ContextMenu}. */
export interface ContextMenuProps extends Readonly<PropsWithChildren> {
  // eslint-disable-next-line @typescript-eslint/naming-convention
  readonly 'aria-label': string
  readonly hidden?: boolean
}

/** Imperative API for {@link ContextMenu}. */
export interface ContextMenuApi {
  readonly open: (position: Pick<MouseEvent, 'pageX' | 'pageY'>) => void
  readonly close: () => void
}

/** A context menu that opens at the current mouse position. */
export const ContextMenu = forwardRef(function ContextMenu(
  props: ContextMenuProps,
  ref: ForwardedRef<ContextMenuApi>,
) {
  const { hidden = false, children } = props

  const root = usePortalContext()
  const [position, setPosition] = useState<Pick<MouseEvent, 'pageX' | 'pageY'> | null>(null)

  useImperativeHandle(ref, () => ({
    open: setPosition,
    close: () => {
      setPosition(null)
    },
  }))

  if (hidden) {
    return children
  }

  return (
    <Popover.Trigger>
      <Pressable>
        <></>
      </Pressable>
      <Popover
        data-testid="context-menu"
        style={{ left: position?.pageX ?? 0, top: position?.pageY ?? 0 }}
        shouldCloseOnInteractOutside={() => true}
        className="sticky flex w-min items-start"
        UNSTABLE_portalContainer={root}
        isOpen={position != null}
        onOpenChange={(isOpen) => {
          if (!isOpen) {
            setPosition(null)
          }
        }}
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
    </Popover.Trigger>
  )
})
