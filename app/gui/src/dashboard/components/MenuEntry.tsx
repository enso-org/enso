/** @file An entry in a menu. */
import * as React from 'react'

import * as detect from 'enso-common/src/detect'
import type * as text from 'enso-common/src/text'

import BlankIcon from '#/assets/blank.svg'

import type * as inputBindings from '#/configurations/inputBindings'

import * as focusHooks from '#/hooks/focusHooks'

import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import KeyboardShortcut from '#/components/dashboard/KeyboardShortcut'
import FocusRing from '#/components/styled/FocusRing'
import SvgMask from '#/components/SvgMask'

import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'
import * as tailwindVariants from '#/utilities/tailwindVariants'

// =================
// === Constants ===
// =================

const MENU_ENTRY_VARIANTS = tailwindVariants.tv({
  base: 'flex h-row grow place-content-between items-center rounded-inherit p-menu-entry text-left selectable group-enabled:active hover:bg-hover-bg disabled:bg-transparent',
  variants: {
    variant: {
      // eslint-disable-next-line @typescript-eslint/naming-convention
      'context-menu': 'px-context-menu-entry-x',
    },
  },
})

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
  rename: 'renameShortcut',
  edit: 'editShortcut',
  editDescription: 'editDescriptionShortcut',
  snapshot: 'snapshotShortcut',
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
  useInNewProject: 'useInNewProjectShortcut',
  closeModal: 'closeModalShortcut',
  cancelEditName: 'cancelEditNameShortcut',
  signIn: 'signInShortcut',
  signOut: 'signOutShortcut',
  downloadApp: 'downloadAppShortcut',
  cancelCut: 'cancelCutShortcut',
  selectAdditional: 'selectAdditionalShortcut',
  selectRange: 'selectRangeShortcut',
  selectAdditionalRange: 'selectAdditionalRangeShortcut',
  goBack: 'goBackShortcut',
  goForward: 'goForwardShortcut',
  aboutThisApp: 'aboutThisAppShortcut',
  openInFileBrowser: 'openInFileBrowserShortcut',
} satisfies { [Key in inputBindings.DashboardBindingKey]: `${Key}Shortcut` }

// =================
// === MenuEntry ===
// =================

/** Props for a {@link MenuEntry}. */
export interface MenuEntryProps extends tailwindVariants.VariantProps<typeof MENU_ENTRY_VARIANTS> {
  readonly icon?: string
  readonly hidden?: boolean
  readonly action: inputBindings.DashboardBindingKey
  /** Overrides the text for the menu entry. */
  readonly label?: string
  /** When true, the button is not clickable. */
  readonly isDisabled?: boolean
  readonly title?: string
  readonly doAction: () => void
}

/** An item in a menu. */
export default function MenuEntry(props: MenuEntryProps) {
  const {
    hidden = false,
    action,
    label,
    isDisabled = false,
    title,
    doAction,
    icon,
    ...variantProps
  } = props
  const { getText } = textProvider.useText()
  const { unsetModal } = modalProvider.useSetModal()
  const dialogContext = ariaComponents.useDialogContext()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const focusChildProps = focusHooks.useFocusChild()
  const info = inputBindings.metadata[action]
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

  React.useEffect(() => {
    // This is slower (but more convenient) than registering every shortcut in the context menu
    // at once.
    if (isDisabled) {
      return
    } else {
      return inputBindings.attach(sanitizedEventTargets.document.body, 'keydown', {
        [action]: doAction,
      })
    }
  }, [isDisabled, inputBindings, action, doAction])

  return hidden ? null : (
      <FocusRing>
        <aria.Button
          {...aria.mergeProps<aria.ButtonProps>()(focusChildProps, {
            isDisabled,
            className: 'group flex w-full rounded-menu-entry',
            onPress: () => {
              if (dialogContext) {
                // Closing a dialog takes precedence over unsetting the modal.
                dialogContext.close()
              } else {
                unsetModal()
              }
              doAction()
            },
          })}
        >
          <div className={MENU_ENTRY_VARIANTS(variantProps)}>
            <div title={title} className="flex items-center gap-menu-entry whitespace-nowrap">
              <SvgMask
                src={icon ?? info.icon ?? BlankIcon}
                color={info.color}
                className="size-4 text-primary"
              />
              <ariaComponents.Text slot="label">
                {label ?? getText(labelTextId)}
              </ariaComponents.Text>
            </div>
            <KeyboardShortcut action={action} />
          </div>
        </aria.Button>
      </FocusRing>
    )
}
