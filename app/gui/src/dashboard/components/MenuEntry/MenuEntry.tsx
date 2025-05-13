/** @file An entry in a menu. */
import BlankIcon from '#/assets/blank.svg'
import { Button } from '#/components/aria'
import type { TextProps } from '#/components/AriaComponents'
import { Text, useDialogContext, useVisualTooltip } from '#/components/AriaComponents'
import KeyboardShortcut from '#/components/dashboard/KeyboardShortcut'
import { Icon } from '#/components/Icon'
import { ACTION_TO_TEXT_ID } from '#/components/MenuEntry/constants'
import FocusRing from '#/components/styled/FocusRing'
import type { DashboardBindingKey } from '#/configurations/inputBindings'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useSyncRef } from '#/hooks/syncRefHooks'
import { useInputBindings } from '#/providers/InputBindingsProvider'
import { unsetModal } from '#/providers/ModalProvider'
import { useText } from '#/providers/TextProvider'
import { document } from '#/utilities/sanitizedEventTargets'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import { isOnMacOS, isOnWindows } from 'enso-common/src/detect'
import type { TextId } from 'enso-common/src/text'
import { useEffect, useRef } from 'react'

const MENU_ENTRY_VARIANTS = tv({
  base: 'flex h-row grow place-content-between items-center rounded-inherit p-menu-entry text-left group-disabled:opacity-30 group-enabled:active group-enabled:hover:bg-hover-bg',
  variants: {
    variant: {
      // eslint-disable-next-line @typescript-eslint/naming-convention
      'context-menu': 'px-context-menu-entry-x',
    },
  },
})

/** Props for a {@link MenuEntry}. */
export interface MenuEntryProps extends VariantProps<typeof MENU_ENTRY_VARIANTS> {
  readonly icon?: string | undefined
  readonly hidden?: boolean | undefined
  readonly action: DashboardBindingKey
  /** Overrides the text for the menu entry. */
  readonly label?: string | undefined
  readonly tooltip?: string | null | undefined
  /** When true, the button is not clickable. */
  readonly isDisabled?: boolean | undefined
  readonly title?: string | undefined
  readonly doAction: () => void
  readonly color?: TextProps['color'] | undefined
}

/** An item in a menu. */
export function MenuEntry(props: MenuEntryProps) {
  const {
    hidden = false,
    action,
    label,
    isDisabled = false,
    title,
    doAction,
    icon,
    tooltip: tooltipValue,
    color,
    ...variantProps
  } = props
  const { getText } = useText()
  const dialogContext = useDialogContext()
  const inputBindings = useInputBindings()
  const info = inputBindings.metadata[action]
  const buttonRef = useRef<HTMLButtonElement>(null)
  const isDisabledRef = useSyncRef(isDisabled)

  const doActionCallback = useEventCallback(() => {
    doAction()
  })

  const labelTextId: TextId = (() => {
    if (action === 'openInFileBrowser') {
      return (
        isOnMacOS() ? 'openInFileBrowserShortcutMacOs'
        : isOnWindows() ? 'openInFileBrowserShortcutWindows'
        : 'openInFileBrowserShortcut'
      )
    } else {
      return ACTION_TO_TEXT_ID[action]
    }
  })()

  useEffect(
    () =>
      inputBindings.attach(document.body, 'keydown', {
        [action]: () => {
          if (isDisabledRef.current) return
          doActionCallback()
        },
      }),
    [inputBindings, action, doActionCallback, isDisabledRef],
  )

  const { tooltip, targetProps } = useVisualTooltip({
    isDisabled: tooltipValue == null,
    targetRef: buttonRef,
    display: 'always',
    children: tooltipValue,
    overlayPositionProps: { placement: 'right' },
  })

  if (hidden) {
    return null
  }

  return (
    <>
      <FocusRing>
        <Button
          ref={buttonRef}
          isDisabled={isDisabled}
          className="group flex w-full rounded-menu-entry"
          onPress={() => {
            if (dialogContext) {
              // Closing a dialog takes precedence over unsetting the modal.
              dialogContext.close()
            } else {
              unsetModal()
            }
            doAction()
          }}
        >
          <div className={MENU_ENTRY_VARIANTS(variantProps)} {...targetProps}>
            <div
              title={title}
              className="flex items-center gap-menu-entry whitespace-nowrap"
              style={{ color: info.color }}
            >
              <Icon
                icon={icon ?? info.icon ?? BlankIcon}
                className={info.color != null ? undefined : 'text-primary'}
              />
              <Text color={color} slot="label">
                {label ?? getText(labelTextId)}
              </Text>
            </div>
            <KeyboardShortcut action={action} />
          </div>
        </Button>
      </FocusRing>
      {tooltip}
    </>
  )
}
