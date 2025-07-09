/** @file A context menu. */
import { Pressable } from '#/components/aria'
import ContextMenuEntry from '#/components/ContextMenuEntry'
import { Popover } from '#/components/Dialog'
import type { MenuEntryProps } from '#/components/MenuEntry'
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
} from 'react'

/** Props for a {@link ContextMenu}. */
export interface ContextMenuProps {
  // eslint-disable-next-line @typescript-eslint/naming-convention
  readonly 'aria-label': string
  readonly entries: readonly (MenuEntryProps | false | null | undefined)[]
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
  const { entries } = props

  const root = usePortalContext()
  const [isOpen, setIsOpen] = useState(false)
  const [position, setPosition] = useState<Pick<MouseEvent, 'pageX' | 'pageY'>>({
    pageX: 0,
    pageY: 0,
  })

  useImperativeHandle(ref, () => ({
    open: (newPosition) => {
      setPosition(newPosition)
      setIsOpen(true)
    },
    close: () => {
      setIsOpen(false)
    },
  }))

  return (
    <Popover.Trigger>
      <Pressable>
        <></>
      </Pressable>
      <Popover
        data-testid="context-menu"
        style={{ left: position.pageX, top: position.pageY }}
        shouldCloseOnInteractOutside={() => true}
        className="sticky flex w-min items-start"
        UNSTABLE_portalContainer={root}
        isOpen={isOpen}
        onOpenChange={setIsOpen}
        onClose={unsetModal}
      >
        <div
          aria-label={props['aria-label']}
          className={twMerge(
            'relative flex flex-col rounded-default',
            isOnMacOS() ? 'w-context-menu-macos' : 'w-context-menu',
          )}
        >
          {entries.flatMap((entry) => {
            if (entry == null || entry === false) {
              return []
            }
            return [<ContextMenuEntry key={entry.action} {...entry} />]
          })}
        </div>
      </Popover>
    </Popover.Trigger>
  )
})
