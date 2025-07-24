/** @file An entry in a menu. */
import BlankIcon from '#/assets/blank.svg'
import LockIcon from '#/assets/lock.svg'
import * as aria from '#/components/aria'
import { useDialogContext } from '#/components/Dialog'
import { Icon } from '#/components/Icon'
import { PaywallDialog } from '#/components/Paywall'
import FocusRing from '#/components/styled/FocusRing'
import { Text, type TextProps } from '#/components/Text'
import { useVisualTooltip } from '#/components/VisualTooltip'
import type * as inputBindings from '#/configurations/inputBindings'
import type { PaywallFeatureName } from '#/hooks/billing'
import KeyboardShortcut from '#/pages/dashboard/components/KeyboardShortcut'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import { setModal, unsetModal } from '#/providers/ModalProvider'
import * as tailwindVariants from '#/utilities/tailwindVariants'
import { useText } from '$/providers/react'
import * as detect from 'enso-common/src/detect'
import type * as text from 'enso-common/src/text'
import * as React from 'react'

const MENU_ENTRY_VARIANTS = tailwindVariants.tv({
  base: 'flex h-row grow place-content-between items-center rounded-inherit p-menu-entry text-left group-disabled:opacity-30 group-enabled:active group-enabled:hover:bg-hover-bg',
  variants: {
    variant: {
      // eslint-disable-next-line @typescript-eslint/naming-convention
      'context-menu': 'px-context-menu-entry-x',
    },
  },
})

/** Get {@link text.TextId} for given shortcut action. */
// All entries are intentionally hard-coded so that `Ctrl+F` will work.
// eslint-disable-next-line react-refresh/only-export-components
export const ACTION_TO_TEXT_ID: Readonly<
  Record<
    inputBindings.DashboardBindingKey,
    Extract<text.TextId, `${inputBindings.DashboardBindingKey}Shortcut`>
  >
> = {
  settings: 'settingsShortcut',
  closeTab: 'closeTabShortcut',
  open: 'openShortcut',
  run: 'runShortcut',
  close: 'closeShortcut',
  uploadToCloud: 'uploadToCloudShortcut',
  downloadToLocal: 'downloadToLocalShortcut',
  rename: 'renameShortcut',
  edit: 'editShortcut',
  delete: 'deleteShortcut',
  undelete: 'undeleteShortcut',
  share: 'shareShortcut',
  label: 'labelShortcut',
  duplicate: 'duplicateShortcut',
  copy: 'copyShortcut',
  copyAsPath: 'copyAsPathShortcut',
  cut: 'cutShortcut',
  paste: 'pasteShortcut',
  download: 'downloadShortcut',
  uploadFiles: 'uploadFilesShortcut',
  newProject: 'newProjectShortcut',
  newFolder: 'newFolderShortcut',
  newDatalink: 'newDatalinkShortcut',
  newSecret: 'newSecretShortcut',
  newCredential: 'newCredentialShortcut',
  useInNewProject: 'useInNewProjectShortcut',
  closeModal: 'closeModalShortcut',
  cancelEditName: 'cancelEditNameShortcut',
  signOut: 'signOutShortcut',
  downloadApp: 'downloadAppShortcut',
  cancelCut: 'cancelCutShortcut',
  selectAdditional: 'selectAdditionalShortcut',
  selectRange: 'selectRangeShortcut',
  selectAdditionalRange: 'selectAdditionalRangeShortcut',
  goBack: 'goBackShortcut',
  goForward: 'goForwardShortcut',
  upgradePlan: 'upgradePlanShortcut',
  aboutThisApp: 'aboutThisAppShortcut',
  openInFileBrowser: 'openInFileBrowserShortcut',
  ensoDevtools: 'ensoDevtoolsShortcut',
  copyId: 'copyIdShortcut',
} satisfies { [Key in inputBindings.DashboardBindingKey]: `${Key}Shortcut` }

/** Props for a {@link MenuEntry}. */
export interface MenuEntryProps extends tailwindVariants.VariantProps<typeof MENU_ENTRY_VARIANTS> {
  readonly icon?: string | undefined
  readonly action: inputBindings.DashboardBindingKey
  /** Overrides the text for the menu entry. */
  readonly label?: string | undefined
  readonly tooltip?: string | null | undefined
  /** When true, the button is not clickable. */
  readonly isDisabled?: boolean | undefined
  readonly title?: string | undefined
  readonly doAction: () => void
  readonly color?: TextProps['color'] | undefined
  readonly isUnderPaywall?: boolean
  readonly feature?: PaywallFeatureName
}

/** An item in a menu. */
export default function MenuEntry(props: MenuEntryProps) {
  const {
    action,
    label,
    isDisabled = false,
    title,
    doAction,
    icon: iconRaw,
    tooltip: tooltipValueRaw,
    color,
    isUnderPaywall = false,
    feature,
    ...variantProps
  } = props

  const { getText } = useText()
  const icon = isUnderPaywall ? LockIcon : iconRaw
  const tooltipValue = isUnderPaywall ? getText('upgradeToUseCloud') : tooltipValueRaw

  const dialogContext = useDialogContext()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const info = inputBindings.metadata[action]
  const buttonRef = React.useRef<HTMLButtonElement>(null)

  const labelTextId: text.TextId = (() => {
    if (action === 'openInFileBrowser') {
      return (
        detect.isOnMacOS() ? 'openInFileBrowserShortcutMacOs'
        : detect.isOnWindows() ? 'openInFileBrowserShortcutWindows'
        : 'openInFileBrowserShortcut'
      )
    } else {
      return ACTION_TO_TEXT_ID[action]
    }
  })()

  const { tooltip, targetProps } = useVisualTooltip({
    isDisabled: tooltipValue == null,
    targetRef: buttonRef,
    display: 'always',
    children: tooltipValue,
    overlayPositionProps: { placement: 'right' },
  })

  return (
    <>
      <FocusRing>
        <aria.Button
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
            if (isUnderPaywall && feature != null) {
              setModal(<PaywallDialog modalProps={{ defaultOpen: true }} feature={feature} />)
            } else {
              doAction()
            }
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
        </aria.Button>
      </FocusRing>
      {tooltip}
    </>
  )
}
