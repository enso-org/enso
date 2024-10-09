/** @file Shortcuts for the dashboard application. */
import * as detect from 'enso-common/src/detect'

import AddDatalinkIcon from '#/assets/add_datalink.svg'
import AddFolderIcon from '#/assets/add_folder.svg'
import AddKeyIcon from '#/assets/add_key.svg'
import AddNetworkIcon from '#/assets/add_network.svg'
import AppDownloadIcon from '#/assets/app_download.svg'
import ArrowLeftIcon from '#/assets/arrow_left.svg'
import ArrowRightIcon from '#/assets/arrow_right.svg'
import CameraIcon from '#/assets/camera.svg'
import CloseIcon from '#/assets/close.svg'
import CloseTabIcon from '#/assets/close_tab.svg'
import CloudToIcon from '#/assets/cloud_to.svg'
import CopyIcon from '#/assets/copy.svg'
import CopyAsPathIcon from '#/assets/copy_as_path.svg'
import DataDownloadIcon from '#/assets/data_download.svg'
import DataUploadIcon from '#/assets/data_upload.svg'
import DuplicateIcon from '#/assets/duplicate.svg'
import LogoIcon from '#/assets/enso_logo.svg'
import OpenIcon from '#/assets/open.svg'
import OpenInFileBrowserIcon from '#/assets/open_in_file_browser.svg'
import PasteIcon from '#/assets/paste.svg'
import PenIcon from '#/assets/pen.svg'
import PeopleIcon from '#/assets/people.svg'
import Play2Icon from '#/assets/play2.svg'
import ScissorsIcon from '#/assets/scissors.svg'
import SettingsIcon from '#/assets/settings.svg'
import SignInIcon from '#/assets/sign_in.svg'
import SignOutIcon from '#/assets/sign_out.svg'
import TagIcon from '#/assets/tag.svg'
import TrashIcon from '#/assets/trash.svg'
import UntrashIcon from '#/assets/untrash.svg'

import * as inputBindings from '#/utilities/inputBindings'

export type * from '#/utilities/inputBindings'

// ======================
// === Input bindings ===
// ======================

/** The type of the keybind and mousebind namespace for the dashboard. */
export type DashboardBindingNamespace = ReturnType<typeof createBindings>

/** The nameof a dashboard binding */
export type DashboardBindingKey = keyof typeof BINDINGS

/** Create a keybind and mousebind namespace. */
export function createBindings() {
  return inputBindings.defineBindingNamespace('dashboard', BINDINGS)
}

export const BINDINGS = inputBindings.defineBindings({
  settings: { name: 'Settings', bindings: ['Mod+,'], icon: SettingsIcon },
  // An alternative shortcut is required because Mod+W cannot be overridden in browsers.
  closeTab: { name: 'Close Tab', bindings: ['Mod+W', 'Mod+Alt+W'], icon: CloseTabIcon },
  open: { name: 'Open', bindings: ['Enter'], icon: OpenIcon },
  run: { name: 'Execute as Task', bindings: ['Shift+Enter'], icon: Play2Icon },
  close: { name: 'Close', bindings: [], icon: CloseIcon },
  uploadToCloud: { name: 'Upload to Cloud', bindings: [], icon: CloudToIcon },
  rename: { name: 'Rename', bindings: ['Mod+R'], icon: PenIcon },
  edit: { name: 'Edit', bindings: ['Mod+E'], icon: PenIcon },
  snapshot: { name: 'Snapshot', bindings: ['Mod+S'], icon: CameraIcon },
  editDescription: { name: 'Edit Description', bindings: ['Mod+Shift+E'], icon: PenIcon },
  delete: {
    name: 'Delete',
    bindings: ['OsDelete'],
    icon: TrashIcon,
    color: 'rgb(243 24 10 / 0.87)',
  },
  undelete: { name: 'Undelete', bindings: ['Mod+R'], icon: UntrashIcon },
  share: { name: 'Share', bindings: ['Mod+Enter'], icon: PeopleIcon },
  label: { name: 'Label', bindings: ['Mod+L'], icon: TagIcon },
  duplicate: { name: 'Duplicate', bindings: ['Mod+D'], icon: DuplicateIcon },
  copy: { name: 'Copy', bindings: ['Mod+C'], icon: CopyIcon },
  copyAsPath: { name: 'Copy as Path', bindings: ['Mod+Shift+C'], icon: CopyAsPathIcon },
  cut: { name: 'Cut', bindings: ['Mod+X'], icon: ScissorsIcon },
  paste: { name: 'Paste', bindings: ['Mod+V'], icon: PasteIcon },
  download: { name: 'Download', bindings: ['Mod+Shift+S'], icon: DataDownloadIcon },
  uploadFiles: { name: 'Upload Files', bindings: ['Mod+U'], icon: DataUploadIcon },
  newProject: { name: 'New Project', bindings: ['Mod+N'], icon: AddNetworkIcon },
  newFolder: { name: 'New Folder', bindings: ['Mod+Shift+N'], icon: AddFolderIcon },
  // FIXME [sb]: Platform detection should be handled directly in `shortcuts.ts`.
  newSecret: {
    name: 'New Secret',
    bindings: !detect.isOnMacOS() ? ['Mod+Alt+N'] : ['Mod+Alt+N', 'Mod+Alt+~'],
    icon: AddKeyIcon,
  },
  newDatalink: {
    name: 'New Datalink',
    bindings: !detect.isOnMacOS() ? ['Mod+Alt+Shift+N'] : ['Mod+Alt+Shift+N', 'Mod+Alt+Shift+~'],
    icon: AddDatalinkIcon,
  },
  useInNewProject: { name: 'Use in New Project', bindings: ['Mod+P'], icon: AddNetworkIcon },
  openInFileBrowser: {
    name: 'Open in File Browser',
    bindings: ['Mod+Shift+O'],
    icon: OpenInFileBrowserIcon,
  },
  signIn: { name: 'Login', bindings: [], icon: SignInIcon },
  signOut: { name: 'Logout', bindings: [], icon: SignOutIcon, color: 'rgb(243 24 10 / 0.87)' },
  // These should not appear in any menus.
  closeModal: { name: 'Close Modal', bindings: ['Escape'], rebindable: false },
  cancelEditName: { name: 'Cancel Editing', bindings: ['Escape'], rebindable: false },
  downloadApp: { name: 'Download App', bindings: [], icon: AppDownloadIcon, rebindable: false },
  cancelCut: { name: 'Cancel Cut', bindings: ['Escape'], rebindable: false },
  // TODO: support handlers for double click; make single click handlers not work on double click events
  // [MouseAction.open]: [mousebind(MouseAction.open, [], MouseButton.left, 2)],
  // [MouseAction.run]: [mousebind(MouseAction.run, ['Shift'], MouseButton.left, 2)],
  selectAdditional: { name: 'Select Additional', bindings: ['Mod+PointerMain'], rebindable: false },
  selectRange: { name: 'Select Range', bindings: ['Shift+PointerMain'], rebindable: false },
  selectAdditionalRange: {
    name: 'Select Additional Range',
    bindings: ['Mod+Shift+PointerMain'],
    rebindable: false,
  },
  goBack: {
    name: 'Go Back',
    bindings: detect.isOnMacOS() ? ['Mod+ArrowLeft', 'Mod+['] : ['Alt+ArrowLeft'],
    rebindable: true,
    icon: ArrowLeftIcon,
  },
  goForward: {
    name: 'Go Forward',
    bindings: detect.isOnMacOS() ? ['Mod+ArrowRight', 'Mod+]'] : ['Alt+ArrowRight'],
    rebindable: true,
    icon: ArrowRightIcon,
  },
  aboutThisApp: {
    name: 'About Enso',
    bindings: ['Mod+/'],
    rebindable: true,
    icon: LogoIcon,
  },
})
