/** @file Shortcuts for the dashboard application. */
import * as inputBindings from '#/utilities/inputBindings'
import * as detect from 'enso-common/src/detect'

/** The type of the keybind and mousebind namespace for the dashboard. */
export type DashboardBindingNamespace = ReturnType<typeof createBindings>

/** The nameof a dashboard binding */
export type DashboardBindingKey = keyof typeof BINDINGS

/** Create a keybind and mousebind namespace. */
export function createBindings() {
  return inputBindings.defineBindingNamespace('dashboard', BINDINGS)
}

export const BINDINGS = inputBindings.defineBindings({
  settings: { bindings: ['Mod+,'], icon: 'settings' },
  open: { bindings: ['Enter'], icon: 'open' },
  run: { bindings: ['Shift+Enter'], icon: 'workflow_play' },
  close: { bindings: [], icon: 'close' },
  uploadToCloud: { bindings: [], icon: 'cloud_to' },
  downloadToLocal: { bindings: [], icon: 'cloud_from' },
  exportArchive: { bindings: [], icon: 'data_download' },
  rename: { bindings: ['Mod+R'], icon: 'edit' },
  edit: { bindings: ['Mod+E'], icon: 'edit' },
  delete: { bindings: ['OsDelete'], icon: 'trash', color: 'rgb(243 24 10 / 0.87)' },
  undelete: { bindings: ['Mod+R'], icon: 'untrash' },
  share: { bindings: ['Mod+Enter'], icon: 'people' },
  label: { bindings: ['Mod+L'], icon: 'tag' },
  duplicate: { bindings: ['Mod+D'], icon: 'duplicate' },
  copy: { bindings: ['Mod+C'], icon: 'copy' },
  copyAsPath: { bindings: ['Mod+Shift+C'], icon: 'copy_as_path' },
  cut: { bindings: ['Mod+X'], icon: 'scissors' },
  paste: { bindings: ['Mod+V'], icon: 'paste' },
  download: { bindings: ['Mod+Shift+S'], icon: 'data_download' },
  uploadFiles: { bindings: ['Mod+U'], icon: 'data_upload' },
  newProject: { bindings: ['Mod+N'], icon: 'graph_add' },
  newFolder: { bindings: ['Mod+Shift+N'], icon: 'folder_add' },
  // FIXME [sb]: Platform detection should be handled directly in `shortcuts.ts`.
  newSecret: {
    bindings: !detect.isOnMacOS() ? ['Mod+Alt+N'] : ['Mod+Alt+N', 'Mod+Alt+~'],
    icon: 'key_add',
  },
  newCredential: { bindings: [], icon: 'credential_add' },
  newDatalink: {
    bindings: !detect.isOnMacOS() ? ['Mod+Alt+Shift+N'] : ['Mod+Alt+Shift+N', 'Mod+Alt+Shift+~'],
    icon: 'connector_add',
  },
  useInNewProject: { bindings: ['Mod+P'], icon: 'graph_add' },
  openInFileBrowser: { bindings: ['Mod+Shift+O'], icon: 'open_in_file_browser' },
  signOut: { bindings: [], icon: 'logout', color: 'rgb(243 24 10 / 0.87)' },
  // These should not appear in any menus.
  closeModal: { bindings: ['Escape'], rebindable: false },
  cancelEditName: { bindings: ['Escape'], rebindable: false },
  downloadApp: { bindings: [], icon: 'data_download', rebindable: false },
  cancelCut: { bindings: ['Escape'], rebindable: false },
  // TODO: support handlers for double click; make single click handlers not work on double click events
  // [MouseAction.open]: [mousebind(MouseAction.open, [], MouseButton.left, 2)],
  // [MouseAction.run]: [mousebind(MouseAction.run, ['Shift'], MouseButton.left, 2)],
  selectAdditional: { bindings: ['Mod+PointerMain'], rebindable: false },
  selectRange: { bindings: ['Shift+PointerMain'], rebindable: false },
  selectAdditionalRange: { bindings: ['Mod+Shift+PointerMain'], rebindable: false },
  goBack: {
    bindings: detect.isOnMacOS() ? ['Mod+ArrowLeft', 'Mod+['] : ['Alt+ArrowLeft'],
    rebindable: true,
    icon: 'arrow_left',
  },
  goForward: {
    bindings: detect.isOnMacOS() ? ['Mod+ArrowRight', 'Mod+]'] : ['Alt+ArrowRight'],
    rebindable: true,
    icon: 'arrow_right',
  },
  upgradePlan: { bindings: [], rebindable: true, icon: 'data_upload' },
  aboutThisApp: { bindings: ['Mod+/'], rebindable: true, icon: 'enso_logo' },
  ensoDevtools: { bindings: [], rebindable: false, icon: 'enso_logo' },
  copyId: { bindings: [], rebindable: false, icon: 'copy_as_path', color: 'rgb(73 159 75)' },
})
