/** @file A registry for keyboard and mouse shortcutManager. */
import type * as React from 'react'

import AddConnectorIcon from 'enso-assets/add_connector.svg'
import AddFolderIcon from 'enso-assets/add_folder.svg'
import AddKeyIcon from 'enso-assets/add_key.svg'
import AddNetworkIcon from 'enso-assets/add_network.svg'
import AppDownloadIcon from 'enso-assets/app_download.svg'
import BlankIcon from 'enso-assets/blank_16.svg'
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

// =================
// === Constants ===
// =================

/** The size (both width and height) of icons. */
export const ICON_SIZE_PX = 16

// =============
// === Types ===
// =============

/** All possible mouse actions for which shortcutManager can be registered. */
export enum MouseAction {
  open = 'open',
  /** Run without opening the editor. */
  run = 'run',
  editName = 'editName',
  selectAdditional = 'selectAdditional',
  selectRange = 'selectRange',
  selectAdditionalRange = 'selectAdditionalRange',
}

/** All possible keyboard actions for which shortcutManager can be registered. */
export enum KeyboardAction {
  settings = 'settings',
  open = 'open',
  /** Run without opening the editor. */
  run = 'run',
  close = 'close',
  uploadToCloud = 'uploadToCloud',
  rename = 'rename',
  edit = 'edit',
  snapshot = 'snapshot',
  moveToTrash = 'moveToTrash',
  moveAllToTrash = 'moveAllToTrash',
  delete = 'delete',
  deleteAll = 'deleteAll',
  restoreFromTrash = 'restoreFromTrash',
  restoreAllFromTrash = 'restoreAllFromTrash',
  share = 'share',
  label = 'label',
  duplicate = 'duplicate',
  copy = 'copy',
  copyAll = 'copyAll',
  cut = 'cut',
  cutAll = 'cutAll',
  cancelCut = 'cancelCut',
  paste = 'paste',
  pasteAll = 'pasteAll',
  download = 'download',
  uploadFiles = 'uploadFiles',
  uploadProjects = 'uploadProjects',
  newProject = 'newProject',
  newFolder = 'newFolder',
  newSecret = 'newSecret',
  newDataLink = 'newDataLink',
  closeModal = 'closeModal',
  cancelEditName = 'cancelEditName',
  changeYourPassword = 'changeYourPassword',
  signIn = 'signIn',
  signOut = 'signOut',
  downloadApp = 'downloadApp',
}

/** Valid mouse buttons. The values of each enum member is its corresponding value of
 * `MouseEvent.button`. */
export enum MouseButton {
  left = 0,
  middle = 1,
  right = 2,
  back = 3,
  forward = 4,
}

/** Restrictions on modifier keys that can trigger a shortcut.
 *
 * If a key is omitted, the shortcut will be triggered regardless of its value in the event. */
interface Modifiers {
  readonly ctrl?: boolean
  readonly alt?: boolean
  readonly shift?: boolean
  readonly meta?: boolean
}

/** A keyboard shortcut. */
export interface KeyboardShortcut extends Modifiers {
  // Every printable character is a valid value for `key`, so unions and enums are both
  // not an option here.
  readonly key: string
  readonly action: KeyboardAction
}

/** A mouse shortcut. If a key is omitted, that means its value does not matter. */
export interface MouseShortcut extends Modifiers {
  readonly button: MouseButton
  readonly action: MouseAction
  readonly clicks: number
}

/** All possible modifier keys. */
export type ModifierKey = (typeof MODIFIERS)[number]

/** A list of all possible modifier keys, in order. */
export const MODIFIERS =
  detect.platform() === detect.Platform.macOS
    ? // These MUST be `as const` to derive the `ModifierKey` type above.
      (['Meta', 'Shift', 'Alt', 'Ctrl'] as const)
    : (['Ctrl', 'Shift', 'Alt', 'Meta'] as const)

// ========================
// === isTextInputEvent ===
// ========================

/** A {@link RegExp} that matches {@link KeyboardEvent.code}s corresponding to non-printable
 * keys. */
const SPECIAL_CHARACTER_KEYCODE_REGEX = /^[A-Z][a-z]/

/** Whether `event` may trigger a shortcut. */
export function isPotentiallyShortcut(event: KeyboardEvent | React.KeyboardEvent) {
  return event.ctrlKey || event.metaKey || event.altKey
}

/** Whether `event.key` is a key used in text editing. */
export function isTextInputKey(event: KeyboardEvent | React.KeyboardEvent) {
  // Allow `alt` key to be pressed in case it is being used to enter special characters.
  return (
    !SPECIAL_CHARACTER_KEYCODE_REGEX.test(event.key) ||
    event.key === 'Backspace' ||
    event.key === 'Delete'
  )
}

/** Whether `event` will produce text. This excludes shortcutManager, as they do not produce text. */
export function isTextInputEvent(event: KeyboardEvent | React.KeyboardEvent) {
  // Allow `alt` key to be pressed in case it is being used to enter special characters.
  return !event.ctrlKey && !event.shiftKey && !event.metaKey && isTextInputKey(event)
}

// =============================
// === makeKeyboardActionMap ===
// =============================

/** Create a mapping from {@link KeyboardAction} to `T`. */
function makeKeyboardActionMap<T>(
  make: (action: KeyboardAction) => T
): Readonly<Record<KeyboardAction, T>> {
  // This is SAFE, as the types of the keys are statically known.
  // eslint-disable-next-line no-restricted-syntax
  return Object.fromEntries(
    Object.entries(KeyboardAction).map(kv => [kv[1], make(kv[1])])
  ) as Record<KeyboardAction, T>
}

// ====================
// === ShortcutInfo ===
// ====================

/** Data needed to render a keyboard shortcut in a context menu. */
export interface ShortcutInfo {
  /** A URL to the image representing this shortcut. */
  readonly icon: string
  /** A Tailwind class for the desired color of the icon. It should be in the form `text-<color>`,
   * where `<color>` is replaced with the actual color. */
  readonly colorClass?: string
}

// ===============================
// === getModifierKeyssOfEvent ===
// ===============================

/** Extracts the list of active {@link ModifierKey}s in an event.
 * This is useful for displaying the modifier keys in the UI. */
export function getModifierKeysOfShortcut(event: KeyboardShortcut | MouseShortcut): ModifierKey[] {
  return detect.isOnMacOS()
    ? [
        // The order SHOULD be Control, Option, Shift, Command. See:
        // https://developer.apple.com/design/human-interface-guidelines/keyboards#Custom-keyboard-shortcutManager
        ...(event.meta === true ? (['Meta'] satisfies ModifierKey[]) : []),
        ...(event.shift === true ? (['Shift'] satisfies ModifierKey[]) : []),
        ...(event.alt === true ? (['Alt'] satisfies ModifierKey[]) : []),
        ...(event.ctrl === true ? (['Ctrl'] satisfies ModifierKey[]) : []),
      ]
    : [
        ...(event.ctrl === true ? (['Ctrl'] satisfies ModifierKey[]) : []),
        ...(event.shift === true ? (['Shift'] satisfies ModifierKey[]) : []),
        ...(event.alt === true ? (['Alt'] satisfies ModifierKey[]) : []),
        ...(event.meta === true ? (['Meta'] satisfies ModifierKey[]) : []),
      ]
}

// ===========================
// === modifiersMatchEvent ===
// ===========================

/** Whether the modifiers match the event's modifier key states. */
function modifiersMatchEvent(
  modifiers: Modifiers,
  event: KeyboardEvent | MouseEvent | React.KeyboardEvent | React.MouseEvent
) {
  return (
    ('ctrl' in modifiers ? event.ctrlKey === modifiers.ctrl : true) &&
    ('alt' in modifiers ? event.altKey === modifiers.alt : true) &&
    ('shift' in modifiers ? event.shiftKey === modifiers.shift : true) &&
    ('meta' in modifiers ? event.metaKey === modifiers.meta : true)
  )
}

// =======================
// === ShortcutManager ===
// =======================

/** Holds all keyboard and mouse shortcutManager, and provides functions to detect them. */
export default class ShortcutManager {
  keyboardShortcutsByKey: Record<string, KeyboardShortcut[]> = {}
  allKeyboardHandlers: Readonly<
    Record<
      KeyboardAction,
      // eslint-disable-next-line @typescript-eslint/no-invalid-void-type
      ((event: KeyboardEvent | React.KeyboardEvent) => boolean | void)[]
    >
  > = makeKeyboardActionMap(() => [])
  /** The last handler (if any) for each action in
   * {@link ShortcutManager.allKeyboardHandlers}. */
  readonly activeKeyboardHandlers: Record<
    KeyboardAction,
    // eslint-disable-next-line @typescript-eslint/no-invalid-void-type
    ((event: KeyboardEvent | React.KeyboardEvent) => boolean | void) | null
  > = makeKeyboardActionMap(() => null)

  /** Create a {@link ShortcutManager}. */
  constructor(
    public readonly keyboardShortcuts: Readonly<Record<KeyboardAction, KeyboardShortcut[]>>,
    public readonly mouseShortcuts: Readonly<Record<MouseAction, MouseShortcut[]>>,
    public readonly keyboardShortcutInfo: Readonly<Record<KeyboardAction, ShortcutInfo>>
  ) {
    this.updateKeyboardShortcutsByKey()
  }

  /** Create a new {@link ShortcutManager} with default values. */
  static createWithDefaults() {
    return new this(
      { ...DEFAULT_KEYBOARD_SHORTCUTS },
      { ...DEFAULT_MOUSE_SHORTCUTS },
      { ...DEFAULT_KEYBOARD_SHORTCUT_INFO }
    )
  }

  /** Return `true` if the shortcut is being triggered by the keyboard event. */
  matchesKeyboardShortcut(
    this: void,
    shortcut: KeyboardShortcut,
    event: KeyboardEvent | React.KeyboardEvent
  ) {
    return (
      shortcut.key.toUpperCase() === event.key.toUpperCase() && modifiersMatchEvent(shortcut, event)
    )
  }

  /** Return `true` if the shortcut is being triggered by the mouse event. */
  matchesMouseShortcut(this: void, shortcut: MouseShortcut, event: MouseEvent | React.MouseEvent) {
    const button: number = shortcut.button
    return (
      button === event.button &&
      event.detail >= shortcut.clicks &&
      modifiersMatchEvent(shortcut, event)
    )
  }

  /** Return `true` if the action is being triggered by the keyboard event. */
  matchesKeyboardAction(action: KeyboardAction, event: KeyboardEvent | React.KeyboardEvent) {
    return this.keyboardShortcuts[action].some(shortcut =>
      this.matchesKeyboardShortcut(shortcut, event)
    )
  }

  /** Return `true` if the action is being triggered by the mouse event. */
  matchesMouseAction(action: MouseAction, event: MouseEvent | React.MouseEvent) {
    return this.mouseShortcuts[action].some(shortcut => this.matchesMouseShortcut(shortcut, event))
  }

  /** Trigger the appropriate handler for the action matching the currently pressed shortcut
   * (if any). Return `true` if a matching action was found, otherwise return `false`. */
  handleKeyboardEvent(event: KeyboardEvent | React.KeyboardEvent) {
    // `event` is missing `.key` on a `keydown` event that fires after signing out.
    // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
    if (event.key != null) {
      for (const shortcut of this.keyboardShortcutsByKey[event.key.toUpperCase()] ?? []) {
        if (this.matchesKeyboardShortcut(shortcut, event)) {
          const handler = this.activeKeyboardHandlers[shortcut.action]
          if (handler != null) {
            const result = handler(event)
            if (result !== false) {
              // The matching `false` return is immediately after this loop.
              // eslint-disable-next-line no-restricted-syntax
              return true
            }
          }
        }
      }
    }
    return false
  }

  /** Regenerate {@link ShortcutManager.keyboardShortcutsByKey}. */
  updateKeyboardShortcutsByKey() {
    this.keyboardShortcutsByKey = {}
    for (const shortcutManager of Object.values(this.keyboardShortcuts)) {
      for (const shortcut of shortcutManager) {
        const byKey = this.keyboardShortcutsByKey[shortcut.key.toUpperCase()]
        if (byKey != null) {
          byKey.unshift(shortcut)
        } else {
          this.keyboardShortcutsByKey[shortcut.key.toUpperCase()] = [shortcut]
        }
      }
    }
  }

  /** Regenerate {@link ShortcutManager.activeKeyboardHandlers}. */
  updateActiveKeyboardHandlers() {
    for (const action of Object.values(KeyboardAction)) {
      const handlers = this.allKeyboardHandlers[action]
      this.activeKeyboardHandlers[action] = handlers[handlers.length - 1] ?? null
    }
  }

  /** Update the currently active handler for each action, and return a function to unregister
   * these handlers. */
  registerKeyboardHandlers(
    handlers: Partial<
      // eslint-disable-next-line @typescript-eslint/no-invalid-void-type
      Record<KeyboardAction, (event: KeyboardEvent | React.KeyboardEvent) => boolean | void>
    >
  ) {
    for (const action of Object.values(KeyboardAction)) {
      const handler = handlers[action]
      if (handler != null) {
        this.allKeyboardHandlers[action].push(handler)
        this.activeKeyboardHandlers[action] = handler
      }
    }
    const allNewHandlers = new Set(Object.values(handlers))
    return () => {
      for (const handlersForCurrentAction of Object.values(this.allKeyboardHandlers)) {
        // Remove in-place the handlers that were added.
        handlersForCurrentAction.splice(
          0,
          handlersForCurrentAction.length,
          ...handlersForCurrentAction.filter(handler => !allNewHandlers.has(handler))
        )
      }
      this.updateActiveKeyboardHandlers()
    }
  }
}

/** A shorthand for creating a {@link KeyboardShortcut}. Should only be used in
 * {@link DEFAULT_KEYBOARD_SHORTCUTS}. */
function keybind(action: KeyboardAction, modifiers: ModifierKey[], key: string): KeyboardShortcut {
  return {
    key,
    action,
    ctrl: modifiers.includes('Ctrl'),
    alt: modifiers.includes('Alt'),
    shift: modifiers.includes('Shift'),
    meta: modifiers.includes('Meta'),
  }
}

/** A shorthand for creating a {@link MouseShortcut}. Should only be used in
 * {@link DEFAULT_MOUSE_SHORTCUTS}. */
function mousebind(
  action: MouseAction,
  modifiers: ModifierKey[],
  button: MouseButton,
  clicks: number
): MouseShortcut {
  return {
    button,
    action,
    clicks,
    ctrl: modifiers.includes('Ctrl'),
    alt: modifiers.includes('Alt'),
    shift: modifiers.includes('Shift'),
    meta: modifiers.includes('Meta'),
  }
}

// =================
// === Constants ===
// =================

/** The equivalent of the `Control` key for the current platform. */
const CTRL = (detect.isOnMacOS() ? 'Meta' : 'Ctrl') satisfies ModifierKey

/** The key known as the `Delete` key for the current platform. */
const DELETE = detect.isOnMacOS() ? 'Backspace' : 'Delete'

/** The default keyboard shortcuts. */
const DEFAULT_KEYBOARD_SHORTCUTS: Readonly<Record<KeyboardAction, KeyboardShortcut[]>> = {
  [KeyboardAction.settings]: [keybind(KeyboardAction.settings, [CTRL], ',')],
  [KeyboardAction.open]: [keybind(KeyboardAction.open, [], 'Enter')],
  [KeyboardAction.run]: [keybind(KeyboardAction.run, ['Shift'], 'Enter')],
  [KeyboardAction.close]: [],
  [KeyboardAction.uploadToCloud]: [],
  [KeyboardAction.rename]: [keybind(KeyboardAction.rename, [CTRL], 'R')],
  [KeyboardAction.edit]: [keybind(KeyboardAction.edit, [CTRL], 'E')],
  [KeyboardAction.snapshot]: [keybind(KeyboardAction.snapshot, [CTRL], 'S')],
  [KeyboardAction.moveToTrash]: [keybind(KeyboardAction.moveToTrash, [], DELETE)],
  [KeyboardAction.moveAllToTrash]: [keybind(KeyboardAction.moveAllToTrash, [], DELETE)],
  [KeyboardAction.delete]: [keybind(KeyboardAction.delete, [], DELETE)],
  [KeyboardAction.deleteAll]: [keybind(KeyboardAction.deleteAll, [], DELETE)],
  [KeyboardAction.restoreFromTrash]: [keybind(KeyboardAction.restoreFromTrash, [CTRL], 'R')],
  [KeyboardAction.restoreAllFromTrash]: [keybind(KeyboardAction.restoreAllFromTrash, [CTRL], 'R')],
  [KeyboardAction.share]: [keybind(KeyboardAction.share, [CTRL], 'Enter')],
  [KeyboardAction.label]: [keybind(KeyboardAction.label, [CTRL], 'L')],
  [KeyboardAction.duplicate]: [keybind(KeyboardAction.duplicate, [CTRL], 'D')],
  [KeyboardAction.copy]: [keybind(KeyboardAction.copy, [CTRL], 'C')],
  [KeyboardAction.copyAll]: [keybind(KeyboardAction.copyAll, [CTRL], 'C')],
  [KeyboardAction.cut]: [keybind(KeyboardAction.cut, [CTRL], 'X')],
  [KeyboardAction.cutAll]: [keybind(KeyboardAction.cutAll, [CTRL], 'X')],
  [KeyboardAction.cancelCut]: [keybind(KeyboardAction.cancelCut, [], 'Escape')],
  [KeyboardAction.paste]: [keybind(KeyboardAction.paste, [CTRL], 'V')],
  [KeyboardAction.pasteAll]: [keybind(KeyboardAction.pasteAll, [CTRL], 'V')],
  [KeyboardAction.download]: [keybind(KeyboardAction.download, [CTRL, 'Shift'], 'S')],
  [KeyboardAction.uploadFiles]: [keybind(KeyboardAction.uploadFiles, [CTRL], 'U')],
  [KeyboardAction.uploadProjects]: [keybind(KeyboardAction.uploadProjects, [CTRL], 'U')],
  [KeyboardAction.newProject]: [keybind(KeyboardAction.newProject, [CTRL], 'N')],
  [KeyboardAction.newFolder]: [keybind(KeyboardAction.newFolder, [CTRL, 'Shift'], 'N')],
  [KeyboardAction.newSecret]: [
    keybind(KeyboardAction.newSecret, [CTRL, 'Alt'], 'N'),
    ...(!detect.isOnMacOS() ? [] : [keybind(KeyboardAction.newSecret, [CTRL, 'Alt'], '~')]),
  ],
  [KeyboardAction.newDataLink]: [
    keybind(KeyboardAction.newDataLink, [CTRL, 'Alt', 'Shift'], 'N'),
    ...(!detect.isOnMacOS()
      ? []
      : [keybind(KeyboardAction.newSecret, [CTRL, 'Alt', 'Shift'], '~')]),
  ],
  [KeyboardAction.closeModal]: [keybind(KeyboardAction.closeModal, [], 'Escape')],
  [KeyboardAction.cancelEditName]: [keybind(KeyboardAction.cancelEditName, [], 'Escape')],
  [KeyboardAction.changeYourPassword]: [],
  [KeyboardAction.signIn]: [],
  [KeyboardAction.signOut]: [],
  [KeyboardAction.downloadApp]: [],
}

/** The default UI data for every keyboard shortcut. */
const DEFAULT_KEYBOARD_SHORTCUT_INFO: Readonly<Record<KeyboardAction, ShortcutInfo>> = {
  [KeyboardAction.settings]: { icon: SettingsIcon },
  [KeyboardAction.open]: { icon: OpenIcon },
  [KeyboardAction.run]: { icon: Play2Icon },
  [KeyboardAction.close]: { icon: CloseIcon },
  [KeyboardAction.uploadToCloud]: { icon: CloudToIcon },
  [KeyboardAction.rename]: { icon: PenIcon },
  [KeyboardAction.edit]: { icon: PenIcon },
  [KeyboardAction.snapshot]: { icon: CameraIcon },
  [KeyboardAction.moveToTrash]: { icon: TrashIcon, colorClass: 'text-delete' },
  [KeyboardAction.moveAllToTrash]: { icon: TrashIcon, colorClass: 'text-delete' },
  [KeyboardAction.delete]: { icon: TrashIcon, colorClass: 'text-delete' },
  [KeyboardAction.deleteAll]: { icon: TrashIcon, colorClass: 'text-delete' },
  [KeyboardAction.restoreFromTrash]: { icon: UntrashIcon },
  [KeyboardAction.restoreAllFromTrash]: { icon: UntrashIcon },
  [KeyboardAction.share]: { icon: PeopleIcon },
  [KeyboardAction.label]: { icon: TagIcon },
  [KeyboardAction.duplicate]: { icon: DuplicateIcon },
  [KeyboardAction.copy]: { icon: CopyIcon },
  [KeyboardAction.copyAll]: { icon: CopyIcon },
  [KeyboardAction.cut]: { icon: ScissorsIcon },
  [KeyboardAction.cutAll]: { icon: ScissorsIcon },
  [KeyboardAction.paste]: { icon: PasteIcon },
  [KeyboardAction.pasteAll]: { icon: PasteIcon },
  [KeyboardAction.download]: { icon: DataDownloadIcon },
  [KeyboardAction.uploadFiles]: { icon: DataUploadIcon },
  [KeyboardAction.uploadProjects]: { icon: DataUploadIcon },
  [KeyboardAction.newProject]: { icon: AddNetworkIcon },
  [KeyboardAction.newFolder]: { icon: AddFolderIcon },
  [KeyboardAction.newSecret]: { icon: AddKeyIcon },
  [KeyboardAction.newDataLink]: { icon: AddConnectorIcon },
  // These should not appear in any context menus.
  [KeyboardAction.closeModal]: { icon: BlankIcon },
  [KeyboardAction.cancelEditName]: { icon: BlankIcon },
  [KeyboardAction.changeYourPassword]: { icon: ChangePasswordIcon },
  [KeyboardAction.signIn]: { icon: SignInIcon },
  [KeyboardAction.signOut]: { icon: SignOutIcon, colorClass: 'text-delete' },
  [KeyboardAction.downloadApp]: { icon: AppDownloadIcon },
  [KeyboardAction.cancelCut]: { icon: BlankIcon },
}

/** The default mouse shortcuts. */
const DEFAULT_MOUSE_SHORTCUTS: Readonly<Record<MouseAction, MouseShortcut[]>> = {
  [MouseAction.open]: [mousebind(MouseAction.open, [], MouseButton.left, 2)],
  [MouseAction.run]: [mousebind(MouseAction.run, ['Shift'], MouseButton.left, 2)],
  [MouseAction.editName]: [mousebind(MouseAction.editName, [CTRL], MouseButton.left, 1)],
  [MouseAction.selectAdditional]: [
    mousebind(MouseAction.selectAdditional, [CTRL], MouseButton.left, 1),
  ],
  [MouseAction.selectRange]: [mousebind(MouseAction.selectRange, ['Shift'], MouseButton.left, 1)],
  [MouseAction.selectAdditionalRange]: [
    mousebind(MouseAction.selectAdditionalRange, [CTRL, 'Shift'], MouseButton.left, 1),
  ],
}
