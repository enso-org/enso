/** @file A context menu. */
import { Pressable } from '#/components/aria'
import ContextMenuEntry from '#/components/ContextMenuEntry'
import { Popover } from '#/components/Dialog'
import type { MenuEntryProps } from '#/components/MenuEntry'
import { usePortalContext } from '#/components/Portal'
import { useEventListener } from '#/hooks/eventListenerHooks'
import { useInputBindings } from '#/providers/InputBindingsProvider'
import { twMerge } from '#/utilities/tailwindMerge'
import { isOnMacOS } from 'enso-common/src/utilities/detect'
import {
  forwardRef,
  useEffect,
  useImperativeHandle,
  useRef,
  useState,
  type ForwardedRef,
  type MouseEvent,
} from 'react'

/** Props for a {@link ContextMenu}. */
export interface ContextMenuProps {
  // eslint-disable-next-line @typescript-eslint/naming-convention
  readonly 'aria-label': string
  readonly entries: readonly (MenuEntryProps | false | null | undefined)[]
  readonly initialPosition?: Pick<MouseEvent, 'pageX' | 'pageY'> | null | undefined
  readonly onClose?: () => void
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
  const { entries, initialPosition, onClose } = props

  const inputBindings = useInputBindings()
  const root = usePortalContext()
  const popoverRef = useRef<HTMLElement>(null)
  const [isOpen, setIsOpen] = useState(initialPosition != null)
  const [position, setPosition] = useState<Pick<MouseEvent, 'pageX' | 'pageY'>>(
    initialPosition ?? { pageX: 0, pageY: 0 },
  )

  useImperativeHandle(ref, () => ({
    open: (newPosition) => {
      setPosition(newPosition)
      setIsOpen(true)
    },
    close: () => {
      setIsOpen(false)
    },
  }))

  useEffect(() => {
    if (!isOpen) return
    return inputBindings.attach(document.body, 'keydown', {
      closeModal: () => {
        setIsOpen(false)
      },
    })
  }, [inputBindings, isOpen])

  useEventListener(
    'scroll',
    (event) => {
      if (
        event.target instanceof Element &&
        popoverRef.current &&
        !popoverRef.current.contains(event.target)
      ) {
        setIsOpen(false)
      }
    },
    document,
    { capture: true },
  )

  useEventListener(
    'contextmenu',
    (event) => {
      if (
        event.target instanceof Element &&
        popoverRef.current &&
        !popoverRef.current.contains(event.target)
      ) {
        setIsOpen(false)
      }
    },
    document,
    { capture: true },
  )

  return (
    <Popover.Trigger>
      <Pressable>
        <></>
      </Pressable>
      <Popover
        data-testid="context-menu"
        // Remove the underlay element to allow scrolling.
        isNonModal
        ref={popoverRef}
        // `position: sticky` must be here rather than in tailwind as `react-aria-components`
        // sets `position: absolute` via `style`.
        style={{ position: 'sticky', left: position.pageX, top: position.pageY }}
        shouldCloseOnInteractOutside={() => true}
        className="flex w-min items-start"
        UNSTABLE_portalContainer={root}
        isOpen={isOpen}
        onOpenChange={setIsOpen}
        onClose={() => {
          onClose?.()
          setIsOpen(false)
        }}
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
