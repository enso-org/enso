/** @file A context menu. */
import { Pressable } from '#/components/aria'
import { Button } from '#/components/Button'
import { Popover } from '#/components/Dialog'
import { Icon } from '#/components/Icon/Icon'
import MenuEntry, { ACTION_TO_TEXT_ID, type MenuEntryProps } from '#/components/MenuEntry'
import { usePortalContext } from '#/components/Portal'
import type { DashboardBindingKey } from '#/configurations/inputBindings'
import { useEventListener } from '#/hooks/eventListenerHooks'
import { useInputBindings } from '#/providers/InputBindingsProvider'
import { twMerge } from '#/utilities/tailwindMerge'
import { useText } from '$/providers/react'
import { isOnMacOS } from 'enso-common/src/detect'
import {
  forwardRef,
  useEffect,
  useImperativeHandle,
  useMemo,
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
  readonly quickActions?: readonly DashboardBindingKey[] | undefined
  readonly initialPosition?: Pick<MouseEvent, 'pageX' | 'pageY'> | null | undefined
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
  const { entries, quickActions, initialPosition } = props

  const inputBindings = useInputBindings()
  const { getText } = useText()
  const root = usePortalContext()
  const popoverRef = useRef<HTMLElement>(null)
  const [isOpen, setIsOpen] = useState(initialPosition != null)
  const [position, setPosition] = useState<Pick<MouseEvent, 'pageX' | 'pageY'>>(
    initialPosition ?? {
      pageX: 0,
      pageY: 0,
    },
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

  const entriesMap = useMemo(
    () =>
      new Map<DashboardBindingKey, MenuEntryProps>(
        entries.flatMap((entry) => {
          if (entry == null || entry === false) return []
          return [[entry.action, entry]]
        }),
      ),
    [entries],
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
          setIsOpen(false)
        }}
      >
        <div
          aria-label={props['aria-label']}
          className={twMerge(
            'relative flex flex-col rounded-default',
            isOnMacOS() ? 'w-[14.375rem]' : 'w-64',
          )}
        >
          {quickActions && (
            <div className="flex h-8 flex-wrap gap-2 overflow-clip px-2.5">
              {quickActions.map((action) => {
                const hasEntry = entriesMap.has(action)
                const entry = entriesMap.get(action) ?? {
                  action,
                  doAction: () => {},
                }
                return (
                  <div
                    key={entry.action}
                    className="grow"
                    style={{ color: inputBindings.metadata[entry.action].color }}
                  >
                    <Button
                      variant="custom"
                      size="medium"
                      className="w-full rounded-lg hover:bg-hover-bg"
                      isDisabled={!hasEntry}
                      tooltip={entry.label ?? getText(ACTION_TO_TEXT_ID[entry.action])}
                      onPress={() => {
                        setIsOpen(false)
                        entry.doAction()
                      }}
                    >
                      <Icon
                        icon={entry.icon ?? inputBindings.metadata[entry.action].icon}
                        className="size-4"
                      />
                    </Button>
                  </div>
                )
              })}
            </div>
          )}
          {entries.flatMap((entry) => {
            if (entry == null || entry === false) {
              return []
            }
            return [<MenuEntry variant="context-menu" key={entry.action} {...entry} />]
          })}
        </div>
      </Popover>
    </Popover.Trigger>
  )
})
