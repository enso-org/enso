/** @file An entry in a menu. */
import * as React from 'react'

import BlankIcon from 'enso-assets/blank.svg'

import type * as text from '#/text'

import type * as inputBindings from '#/configurations/inputBindings'

import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import KeyboardShortcut from '#/components/dashboard/KeyboardShortcut'
import SvgMask from '#/components/SvgMask'
import UnstyledButton from '#/components/UnstyledButton'

import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'

// =================
// === Constants ===
// =================

const ACTION_TO_TEXT_ID: Readonly<Record<inputBindings.DashboardBindingKey, text.TextId>> = {
  settings: 'settingsShortcut',
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
  cut: 'cutShortcut',
  paste: 'pasteShortcut',
  download: 'downloadShortcut',
  uploadFiles: 'uploadFilesShortcut',
  uploadProjects: 'uploadProjectsShortcut',
  newProject: 'newProjectShortcut',
  newFolder: 'newFolderShortcut',
  newDataLink: 'newDataLinkShortcut',
  newSecret: 'newSecretShortcut',
  useInNewProject: 'useInNewProjectShortcut',
  closeModal: 'closeModalShortcut',
  cancelEditName: 'cancelEditNameShortcut',
  signIn: 'signInShortcut',
  signOut: 'signOutShortcut',
  downloadApp: 'downloadAppShortcut',
  cancelCut: 'cancelCutShortcut',
  editName: 'editNameShortcut',
  selectAdditional: 'selectAdditionalShortcut',
  selectRange: 'selectRangeShortcut',
  selectAdditionalRange: 'selectAdditionalRangeShortcut',
  goBack: 'goBackShortcut',
  goForward: 'goForwardShortcut',
  aboutThisApp: 'aboutThisAppShortcut',
} satisfies { [Key in inputBindings.DashboardBindingKey]: `${Key}Shortcut` }

// =================
// === MenuEntry ===
// =================

/** Props for a {@link MenuEntry}. */
export interface MenuEntryProps {
  readonly hidden?: boolean
  readonly action: inputBindings.DashboardBindingKey
  /** Overrides the text for the menu entry. */
  readonly label?: string
  /** When true, the button is not clickable. */
  readonly isDisabled?: boolean
  readonly title?: string
  readonly isContextMenuEntry?: boolean
  readonly doAction: () => void
}

/** An item in a menu. */
export default function MenuEntry(props: MenuEntryProps) {
  const { hidden = false, action, label, isDisabled = false, title } = props
  const { isContextMenuEntry = false, doAction } = props
  const { getText } = textProvider.useText()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const info = inputBindings.metadata[action]
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
    <UnstyledButton
      isDisabled={isDisabled}
      className="group flex w-full rounded-menu-entry"
      onPress={doAction}
    >
      <div
        className={`flex h-row grow place-content-between items-center rounded-inherit p-menu-entry text-left selectable group-enabled:active hover:bg-hover-bg disabled:bg-transparent ${
          isContextMenuEntry ? 'px-context-menu-entry-x' : ''
        }`}
      >
        <div title={title} className="flex items-center gap-menu-entry whitespace-nowrap">
          <SvgMask src={info.icon ?? BlankIcon} color={info.color} className="size-icon" />
          <aria.Text slot="label">{label ?? getText(ACTION_TO_TEXT_ID[action])}</aria.Text>
        </div>
        <KeyboardShortcut action={action} />
      </div>
    </UnstyledButton>
  )
}
