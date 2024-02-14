/** @file Shortcuts for the dashboard application. */

import AddConnectorIcon from 'enso-assets/add_connector.svg'
import AddFolderIcon from 'enso-assets/add_folder.svg'
import AddKeyIcon from 'enso-assets/add_key.svg'
import AddNetworkIcon from 'enso-assets/add_network.svg'
import AppDownloadIcon from 'enso-assets/app_download.svg'
import CameraIcon from 'enso-assets/camera.svg'
import ChangePasswordIcon from 'enso-assets/change_password.svg'
import CloseIcon from 'enso-assets/close.svg'
import CloudToIcon from 'enso-assets/cloud_to.svg'
import CopyIcon from 'enso-assets/copy.svg'
import DataDownloadIcon from 'enso-assets/data_download.svg'
import DataUploadIcon from 'enso-assets/data_upload.svg'
import DuplicateIcon from 'enso-assets/duplicate.svg'
import OpenIcon from 'enso-assets/open.svg'
import PasteIcon from 'enso-assets/paste.svg'
import PenIcon from 'enso-assets/pen.svg'
import PeopleIcon from 'enso-assets/people.svg'
import Play2Icon from 'enso-assets/play2.svg'
import ScissorsIcon from 'enso-assets/scissors.svg'
import SettingsIcon from 'enso-assets/settings.svg'
import SignInIcon from 'enso-assets/sign_in.svg'
import SignOutIcon from 'enso-assets/sign_out.svg'
import TagIcon from 'enso-assets/tag.svg'
import TrashIcon from 'enso-assets/trash.svg'
import UntrashIcon from 'enso-assets/untrash.svg'
import * as detect from 'enso-common/src/detect'

import * as inputBindings from '#/utilities/inputBindings'

export type * from '#/utilities/inputBindings'

// ======================
// === Input bindings ===
// ======================

/** The type of the keybind and mousebind namespace for the dashboard. */
export interface DashboardBindingNamespace extends ReturnType<typeof createBindings> {}

/** The nameof a dashboard binding */
export type DashboardBindingKey = keyof typeof BINDINGS

/** Create a keybind and mousebind namespace. */
export function createBindings() {
  return inputBindings.defineBindingNamespace('dashboard', BINDINGS)
}

export const BINDINGS = inputBindings.defineBindings({
  settings: { name: 'Settings', bindings: ['Mod+,'], icon: SettingsIcon },
  open: { name: 'Open', bindings: ['Enter'], icon: OpenIcon },
  run: { name: 'Run', bindings: ['Shift+Enter'], icon: Play2Icon },
  close: { name: 'Close', bindings: [], icon: CloseIcon },
  uploadToCloud: { name: 'Upload To Cloud', bindings: [], icon: CloudToIcon },
  rename: { name: 'Rename', bindings: ['Mod+R'], icon: PenIcon },
  edit: { name: 'Edit', bindings: ['Mod+E'], icon: PenIcon },
  snapshot: { name: 'Snapshot', bindings: ['Mod+S'], icon: CameraIcon },
  moveToTrash: {
    name: 'Move To Trash',
    bindings: ['OsDelete'],
    icon: TrashIcon,
    color: 'rgba(243, 24, 10, 0.87)',
  },
  moveAllToTrash: {
    name: 'Move All To Trash',
    bindings: ['OsDelete'],
    icon: TrashIcon,
    color: 'rgba(243, 24, 10, 0.87)',
  },
  delete: {
    name: 'Delete',
    bindings: ['OsDelete'],
    icon: TrashIcon,
    color: 'rgba(243, 24, 10, 0.87)',
  },
  deleteAll: {
    name: 'Delete All',
    bindings: ['OsDelete'],
    icon: TrashIcon,
    color: 'rgba(243, 24, 10, 0.87)',
  },
  restoreFromTrash: { name: 'Restore From Trash', bindings: ['Mod+R'], icon: UntrashIcon },
  restoreAllFromTrash: { name: 'Restore All From Trash', bindings: ['Mod+R'], icon: UntrashIcon },
  share: { name: 'Share', bindings: ['Mod+Enter'], icon: PeopleIcon },
  label: { name: 'Label', bindings: ['Mod+L'], icon: TagIcon },
  duplicate: { name: 'Duplicate', bindings: ['Mod+D'], icon: DuplicateIcon },
  copy: { name: 'Copy', bindings: ['Mod+C'], icon: CopyIcon },
  copyAll: { name: 'Copy All', bindings: ['Mod+C'], icon: CopyIcon },
  cut: { name: 'Cut', bindings: ['Mod+X'], icon: ScissorsIcon },
  cutAll: { name: 'Cut All', bindings: ['Mod+X'], icon: ScissorsIcon },
  paste: { name: 'Paste', bindings: ['Mod+V'], icon: PasteIcon },
  pasteAll: { name: 'Paste All', bindings: ['Mod+V'], icon: PasteIcon },
  download: { name: 'Download', bindings: ['Mod+Shift+S'], icon: DataDownloadIcon },
  uploadFiles: { name: 'Upload Files', bindings: ['Mod+U'], icon: DataUploadIcon },
  uploadProjects: { name: 'Upload Projects', bindings: ['Mod+U'], icon: DataUploadIcon },
  newProject: { name: 'New Project', bindings: ['Mod+N'], icon: AddNetworkIcon },
  newFolder: { name: 'New Folder', bindings: ['Mod+Shift+N'], icon: AddFolderIcon },
  // FIXME [sb]: Platform detection should be handled directly in `shortcuts.ts`.
  newSecret: {
    name: 'New Secret',
    bindings: !detect.isOnMacOS() ? ['Mod+Alt+N'] : ['Mod+Alt+N', 'Mod+Alt+~'],
    icon: AddKeyIcon,
  },
  newDataLink: {
    name: 'New Data Link',
    bindings: !detect.isOnMacOS() ? ['Mod+Alt+Shift+N'] : ['Mod+Alt+Shift+N', 'Mod+Alt+Shift+~'],
    icon: AddConnectorIcon,
  },
  signIn: { name: 'Login', bindings: [], icon: SignInIcon },
  signOut: { name: 'Logout', bindings: [], icon: SignOutIcon, color: 'rgba(243, 24, 10, 0.87)' },
  // These should not appear in any menus.
  closeModal: { name: 'Close Modal', bindings: ['Escape'] },
  cancelEditName: { name: 'Cancel Editing', bindings: ['Escape'] },
  changeYourPassword: { name: 'Change Your Password', bindings: [], icon: ChangePasswordIcon },
  downloadApp: { name: 'Download App', bindings: [], icon: AppDownloadIcon },
  cancelCut: { name: 'Cancel Cut', bindings: ['Escape'] },
  // TODO: support handlers for double click; make single click handlers not work on double click events
  // [MouseAction.open]: [mousebind(MouseAction.open, [], MouseButton.left, 2)],
  // [MouseAction.run]: [mousebind(MouseAction.run, ['Shift'], MouseButton.left, 2)],
  editName: ['Mod+PointerMain'],
  selectAdditional: ['Mod+PointerMain'],
  selectRange: ['Shift+PointerMain'],
  selectAdditionalRange: ['Mod+Shift+PointerMain'],
})
