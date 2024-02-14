/** @file Shortcuts for the dashboard application. */

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
  settings: ['Mod+,'],
  open: ['Enter'],
  run: ['Shift+Enter'],
  close: [],
  uploadToCloud: [],
  rename: ['Mod+R'],
  edit: ['Mod+E'],
  snapshot: ['Mod+S'],
  moveToTrash: ['OsDelete'],
  moveAllToTrash: ['OsDelete'],
  delete: ['OsDelete'],
  deleteAll: ['OsDelete'],
  restoreFromTrash: ['Mod+R'],
  restoreAllFromTrash: ['Mod+R'],
  share: ['Mod+Enter'],
  label: ['Mod+L'],
  duplicate: ['Mod+D'],
  copy: ['Mod+C'],
  copyAll: ['Mod+C'],
  cut: ['Mod+X'],
  cutAll: ['Mod+X'],
  cancelCut: ['Escape'],
  paste: ['Mod+V'],
  pasteAll: ['Mod+V'],
  download: ['Mod+Shift+S'],
  uploadFiles: ['Mod+U'],
  uploadProjects: ['Mod+U'],
  newProject: ['Mod+N'],
  newFolder: ['Mod+Shift+N'],
  // FIXME [sb]: Platform detection should be handled directly in `shortcuts.ts`.
  newSecret: !detect.isOnMacOS() ? ['Mod+Alt+N'] : ['Mod+Alt+N', 'Mod+Alt+~'],
  newDataLink: !detect.isOnMacOS() ? ['Mod+Alt+Shift+N'] : ['Mod+Alt+Shift+N', 'Mod+Alt+Shift+~'],
  closeModal: ['Escape'],
  cancelEditName: ['Escape'],
  changeYourPassword: [],
  signIn: [],
  signOut: [],
  downloadApp: [],
  // TODO: support handlers for double click; make single click handlers not work on double click events
  // [MouseAction.open]: [mousebind(MouseAction.open, [], MouseButton.left, 2)],
  // [MouseAction.run]: [mousebind(MouseAction.run, ['Shift'], MouseButton.left, 2)],
  editName: ['Mod+PointerMain'],
  selectAdditional: ['Mod+PointerMain'],
  selectRange: ['Shift+PointerMain'],
  selectAdditionalRange: ['Mod+Shift+PointerMain'],
})
