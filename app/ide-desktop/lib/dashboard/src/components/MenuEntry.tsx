/** @file An entry in a menu. */
import * as React from 'react'

import BlankIcon from 'enso-assets/blank.svg'

import type * as text from '#/text'

import type * as inputBindings from '#/configurations/inputBindings'

import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as textProvider from '#/providers/TextProvider'

import KeyboardShortcut from '#/components/dashboard/KeyboardShortcut'
import SvgMask from '#/components/SvgMask'

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
  readonly disabled?: boolean
  readonly title?: string
  readonly isContextMenuEntry?: boolean
  readonly doAction: () => void
}

/** An item in a menu. */
export default function MenuEntry(props: MenuEntryProps) {
  const {
    hidden = false,
    action,
    label,
    disabled = false,
    title,
    isContextMenuEntry = false,
  } = props
  const { doAction } = props
  const { getText } = textProvider.useText()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const info = inputBindings.metadata[action]
  React.useEffect(() => {
    // This is slower (but more convenient) than registering every shortcut in the context menu
    // at once.
    if (disabled) {
      return
    } else {
      return inputBindings.attach(sanitizedEventTargets.document.body, 'keydown', {
        [action]: doAction,
      })
    }
  }, [disabled, inputBindings, action, doAction])

  return hidden ? null : (
    <button
      disabled={disabled}
      title={title}
      className={`items -center flex h-row
      place-content-between rounded-menu-entry p-menu-entry text-left selectable enabled:active hover:bg-hover-bg disabled:bg-transparent ${
        isContextMenuEntry ? 'px-context-menu-entry-x' : ''
      }`}
      onClick={event => {
        event.stopPropagation()
        doAction()
      }}
    >
      <div className="flex items-center gap-menu-entry whitespace-nowrap">
        <SvgMask src={info.icon ?? BlankIcon} color={info.color} className="size-icon" />
        {label ?? getText(ACTION_TO_TEXT_ID[action])}
      </div>
      <KeyboardShortcut action={action} />
    </button>
  )
}
