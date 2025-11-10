/** @file Shortcuts for the dashboard application. */
import { SETTINGS_TAB_DATA } from '#/layouts/Settings/data'
import * as inputBindings from '#/utilities/inputBindings'
import * as detect from 'enso-common/src/utilities/detect'

/** The type of the keybind and mousebind namespace for the dashboard. */
export type DashboardBindingNamespace = ReturnType<typeof createBindings>

/**
 * The categories of dashboard bindings.
 * These are used to group bindings in the UI.
 */
export type DashboardBindingCategory = (typeof CATEGORIES)[number]

/** The nameof a dashboard binding */
export type DashboardBindingKey = keyof typeof BINDINGS

/** Create a keybind and mousebind namespace. */
export function createBindings() {
  return inputBindings.defineBindingNamespace('dashboard', BINDINGS, CATEGORIES)
}

const BINDINGS_AND_CATEGORIES = inputBindings.defineBindings(
  [
    'help',
    'other',
    'fileManagement',
    'editing',
    'collaboration',
    'settings',
    'navigation',
    'developer',
  ],
  {
    settings: { bindings: ['Mod+,'], icon: 'settings', category: 'navigation' },
    open: { bindings: ['Enter'], icon: 'open', category: 'other' },
    run: { bindings: ['Shift+Enter'], icon: 'workflow_play', category: 'other' },
    close: { bindings: [], icon: 'close', category: 'other' },
    uploadToCloud: { bindings: [], icon: 'cloud_to', category: 'fileManagement' },
    downloadToLocal: { bindings: [], icon: 'cloud_from', category: 'fileManagement' },
    exportArchive: { bindings: [], icon: 'data_download', category: 'fileManagement' },
    rename: { bindings: ['Mod+R'], icon: 'edit', category: 'editing' },
    edit: { bindings: ['Mod+E'], icon: 'edit', category: 'editing' },
    delete: {
      bindings: ['OsDelete'],
      icon: 'trash',
      color: 'rgb(243 24 10 / 0.87)',
      category: 'fileManagement',
    },
    undelete: { bindings: ['Mod+R'], icon: 'untrash', category: 'fileManagement' },
    share: { bindings: ['Mod+Enter'], icon: 'people', category: 'collaboration' },
    label: { bindings: ['Mod+L'], icon: 'tag', category: 'collaboration' },
    duplicate: { bindings: ['Mod+D'], icon: 'duplicate', category: 'fileManagement' },
    copy: { bindings: ['Mod+C'], icon: 'copy', category: 'fileManagement' },
    copyAsPath: { bindings: ['Mod+Shift+C'], icon: 'copy_as_path', category: 'fileManagement' },
    cut: { bindings: ['Mod+X'], icon: 'scissors', category: 'fileManagement' },
    paste: { bindings: ['Mod+V'], icon: 'paste', category: 'fileManagement' },
    download: { bindings: ['Mod+Shift+S'], icon: 'data_download', category: 'fileManagement' },
    uploadFiles: { bindings: ['Mod+U'], icon: 'data_upload', category: 'fileManagement' },
    newProject: { bindings: ['Mod+N'], icon: 'graph_add', category: 'fileManagement' },
    newFolder: { bindings: ['Mod+Shift+N'], icon: 'folder_add', category: 'fileManagement' },
    // FIXME [sb]: Platform detection should be handled directly in `shortcuts.ts`.
    newSecret: {
      bindings: !detect.isOnMacOS() ? ['Mod+Alt+N'] : ['Mod+Alt+N', 'Mod+Alt+~'],
      icon: 'key_add',
      category: 'fileManagement',
    },
    newCredential: { bindings: [], icon: 'credential_add', category: 'fileManagement' },
    newDatalink: {
      bindings: !detect.isOnMacOS() ? ['Mod+Alt+Shift+N'] : ['Mod+Alt+Shift+N', 'Mod+Alt+Shift+~'],
      icon: 'connector_add',
      category: 'fileManagement',
    },
    useInNewProject: { bindings: ['Mod+P'], icon: 'graph_add', category: 'fileManagement' },
    openInFileBrowser: {
      bindings: ['Mod+Shift+O'],
      icon: 'open_in_file_browser',
      category: 'other',
    },
    signOut: { bindings: [], icon: 'logout', color: 'rgb(243 24 10 / 0.87)', category: 'other' },
    // These should not appear in any menus.
    closeModal: { bindings: ['Escape'], rebindable: false, category: 'other' },
    cancelEditName: { bindings: ['Escape'], rebindable: false, category: 'editing' },
    downloadApp: { bindings: [], icon: 'data_download', rebindable: false, category: 'other' },
    cancelCut: { bindings: ['Escape'], rebindable: false, category: 'fileManagement' },
    // TODO: support handlers for double click; make single click handlers not work on double click events
    // [MouseAction.open]: [mousebind(MouseAction.open, [], MouseButton.left, 2)],
    // [MouseAction.run]: [mousebind(MouseAction.run, ['Shift'], MouseButton.left, 2)],
    selectAdditional: { bindings: ['Mod+PointerMain'], rebindable: false, category: 'other' },
    selectRange: { bindings: ['Shift+PointerMain'], rebindable: false, category: 'other' },
    selectAdditionalRange: {
      bindings: ['Mod+Shift+PointerMain'],
      rebindable: false,
      category: 'other',
    },
    goBack: {
      bindings: detect.isOnMacOS() ? ['Mod+ArrowLeft', 'Mod+['] : ['Alt+ArrowLeft'],
      rebindable: true,
      icon: 'arrow_left',
      category: 'navigation',
    },
    goForward: {
      bindings: detect.isOnMacOS() ? ['Mod+ArrowRight', 'Mod+]'] : ['Alt+ArrowRight'],
      rebindable: true,
      icon: 'arrow_right',
      category: 'navigation',
    },
    upgradePlan: { bindings: [], icon: 'data_upload', category: 'other' },
    aboutThisApp: { bindings: ['Mod+/'], icon: 'enso_logo', category: 'help' },
    toggleEnsoDevtools: {
      bindings: [],
      rebindable: false,
      icon: 'enso_logo',
      category: 'developer',
    },
    goToAccountSettings: {
      bindings: [],
      icon: SETTINGS_TAB_DATA.account.icon,
      category: 'settings',
    },
    goToOrganizationSettings: {
      bindings: [],
      icon: SETTINGS_TAB_DATA.organization.icon,
      category: 'settings',
    },
    goToLocalSettings: { bindings: [], icon: SETTINGS_TAB_DATA.local.icon, category: 'settings' },
    goToBillingAndPlansSettings: {
      bindings: [],
      icon: SETTINGS_TAB_DATA['billing-and-plans'].icon,
      category: 'settings',
    },
    goToMembersSettings: {
      bindings: [],
      icon: SETTINGS_TAB_DATA.members.icon,
      category: 'settings',
    },
    goToUserGroupsSettings: {
      bindings: [],
      icon: SETTINGS_TAB_DATA['user-groups'].icon,
      category: 'settings',
    },
    goToKeyboardShortcutsSettings: {
      bindings: [],
      icon: SETTINGS_TAB_DATA['keyboard-shortcuts'].icon,
      category: 'settings',
    },
    goToActivityLogSettings: {
      bindings: [],
      icon: SETTINGS_TAB_DATA['activity-log'].icon,
      category: 'settings',
    },
    copyId: {
      bindings: [],
      rebindable: false,
      icon: 'copy_as_path',
      color: 'rgb(73 159 75)',
      category: 'developer',
    },
  },
)

export const BINDINGS = BINDINGS_AND_CATEGORIES.bindings
export const CATEGORIES = BINDINGS_AND_CATEGORIES.categories
