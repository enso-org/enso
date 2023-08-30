/** @file A registry for keyboard and mouse shortcuts. */
import * as React from 'react'

import AddConnectorIcon from 'enso-assets/add_connector.svg'
import AddFolderIcon from 'enso-assets/add_folder.svg'
import AddNetworkIcon from 'enso-assets/add_network.svg'
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
import PenIcon from 'enso-assets/pen.svg'
import PeopleIcon from 'enso-assets/people.svg'
import ScissorsIcon from 'enso-assets/scissors.svg'
import SignInIcon from 'enso-assets/sign_in.svg'
import SignOutIcon from 'enso-assets/sign_out.svg'
import TagIcon from 'enso-assets/tag.svg'
import TrashIcon from 'enso-assets/trash.svg'

import * as detect from 'enso-common/src/detect'

// This file MUST be a `.tsx` file so that Tailwind includes the CSS classes used here.

// =================
// === Constants ===
// =================

/** The size (both width and height) of icons. */
export const ICON_SIZE_PX = 16

// =============
// === Types ===
// =============

/** All possible mouse actions for which shortcuts can be registered. */
export enum MouseAction {
    editName = 'edit-name',
    selectAdditional = 'select-additional',
    selectRange = 'select-range',
    selectAdditionalRange = 'select-additional-range',
}

/** All possible keyboard actions for which shortcuts can be registered. */
export enum KeyboardAction {
    open = 'open',
    close = 'close',
    uploadToCloud = 'upload-to-cloud',
    rename = 'rename',
    snapshot = 'snapshot',
    moveToTrash = 'move-to-trash',
    moveAllToTrash = 'move-all-to-trash',
    share = 'share',
    label = 'label',
    duplicate = 'duplicate',
    copy = 'copy',
    cut = 'cut',
    download = 'download',
    uploadFiles = 'upload-files',
    newProject = 'new-project',
    newFolder = 'new-folder',
    newDataConnector = 'new-data-connector',
    closeModal = 'close-modal',
    cancelEditName = 'cancel-edit-name',
    changeYourPassword = 'change-your-password',
    signIn = 'sign-in',
    signOut = 'sign-out',
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
    ctrl?: boolean
    alt?: boolean
    shift?: boolean
    meta?: boolean
}

/** A keyboard shortcut. */
export interface KeyboardShortcut extends Modifiers {
    // Every printable character is a valid value for `key`, so unions and enums are both
    // not an option here.
    key: string
    action: KeyboardAction
}

/** A mouse shortcut. If a key is omitted, that means its value does not matter. */
export interface MouseShortcut extends Modifiers {
    button: MouseButton
    action: MouseAction
}

/** All possible modifier keys. */
export type ModifierKey = (typeof MODIFIERS)[number]

/** A list of all possible modifier keys, in order. */
export const MODIFIERS =
    detect.platform() === detect.Platform.macOS
        ? // This is required to derive the `ModifierKey` type above.
          // eslint-disable-next-line no-restricted-syntax
          (['Meta', 'Shift', 'Alt', 'Ctrl'] as const)
        : // eslint-disable-next-line no-restricted-syntax
          (['Ctrl', 'Shift', 'Alt', 'Meta'] as const)

// ========================
// === isTextInputEvent ===
// ========================

/** A {@link RegExp} that matches {@link KeyboardEvent.code}s corresponding to non-printable
 * keys. */
const SPECIAL_CHARACTER_KEYCODE_REGEX = /^[A-Z][a-z]/

/** Whether the modifiers match the event's modifier key states. */
export function isTextInputEvent(event: KeyboardEvent | React.KeyboardEvent) {
    // Allow `alt` key to be pressed in case it is being used to enter special characters.
    return (
        !event.ctrlKey &&
        !event.shiftKey &&
        !event.metaKey &&
        (!SPECIAL_CHARACTER_KEYCODE_REGEX.test(event.key) ||
            event.key === 'Backspace' ||
            event.key === 'Delete')
    )
}

// =============================
// === makeKeyboardActionMap ===
// =============================

/** Create a mapping from {@link KeyboardAction} to `T`. */
function makeKeyboardActionMap<T>(make: () => T): Record<KeyboardAction, T> {
    return {
        [KeyboardAction.open]: make(),
        [KeyboardAction.close]: make(),
        [KeyboardAction.uploadToCloud]: make(),
        [KeyboardAction.rename]: make(),
        [KeyboardAction.snapshot]: make(),
        [KeyboardAction.moveToTrash]: make(),
        [KeyboardAction.moveAllToTrash]: make(),
        [KeyboardAction.share]: make(),
        [KeyboardAction.label]: make(),
        [KeyboardAction.duplicate]: make(),
        [KeyboardAction.copy]: make(),
        [KeyboardAction.cut]: make(),
        [KeyboardAction.download]: make(),
        [KeyboardAction.uploadFiles]: make(),
        [KeyboardAction.newProject]: make(),
        [KeyboardAction.newFolder]: make(),
        [KeyboardAction.newDataConnector]: make(),
        [KeyboardAction.closeModal]: make(),
        [KeyboardAction.cancelEditName]: make(),
        [KeyboardAction.changeYourPassword]: make(),
        [KeyboardAction.signIn]: make(),
        [KeyboardAction.signOut]: make(),
    }
}

// ====================
// === ShortcutInfo ===
// ====================

/** Data needed to render a keyboard shortcut in a context menu. */
export interface ShortcutInfo {
    name: string
    /** A URL to the image representing this shortcut. */
    icon: string
    /** A Tailwind class for the desired color of the icon. It should be in the form `text-<color>`,
     * where `<color>` is replaced with the actual color. */
    colorClass?: string
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
              // https://developer.apple.com/design/human-interface-guidelines/keyboards#Custom-keyboard-shortcuts
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

// ========================
// === ShortcutRegistry ===
// ========================

/** Holds all keyboard and mouse shortcuts, and provides functions to detect them. */
export class ShortcutRegistry {
    keyboardShortcutsByKey: Record<string, KeyboardShortcut[]> = {}
    allKeyboardHandlers: Record<
        KeyboardAction,
        ((event: KeyboardEvent | React.KeyboardEvent) => void)[]
    > = makeKeyboardActionMap(() => [])
    /** The last handler (if any) for each action in
     * {@link ShortcutRegistry.allKeyboardHandlers}. */
    activeKeyboardHandlers: Record<
        KeyboardAction,
        ((event: KeyboardEvent | React.KeyboardEvent) => void) | null
    > = makeKeyboardActionMap(() => null)

    /** Create a {@link ShortcutRegistry}. */
    constructor(
        public keyboardShortcuts: Record<KeyboardAction, KeyboardShortcut[]>,
        public mouseShortcuts: Record<MouseAction, MouseShortcut[]>,
        public keyboardShortcutInfo: Record<KeyboardAction, ShortcutInfo>
    ) {
        this.updateKeyboardShortcutsByKey()
    }

    /** Create a new {@link ShortcutRegistry} with default values. */
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
            shortcut.key.toUpperCase() === event.key.toUpperCase() &&
            modifiersMatchEvent(shortcut, event)
        )
    }

    /** Return `true` if the shortcut is being triggered by the mouse event. */
    matchesMouseShortcut(
        this: void,
        shortcut: MouseShortcut,
        event: MouseEvent | React.MouseEvent
    ) {
        return shortcut.button === event.button && modifiersMatchEvent(shortcut, event)
    }

    /** Return `true` if the action is being triggered by the keyboard event. */
    matchesKeyboardAction(action: KeyboardAction, event: KeyboardEvent | React.KeyboardEvent) {
        return this.keyboardShortcuts[action].some(shortcut =>
            this.matchesKeyboardShortcut(shortcut, event)
        )
    }

    /** Return `true` if the action is being triggered by the mouse event. */
    matchesMouseAction(action: MouseAction, event: MouseEvent | React.MouseEvent) {
        return this.mouseShortcuts[action].some(shortcut =>
            this.matchesMouseShortcut(shortcut, event)
        )
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
                        handler(event)
                        // The matching `false` return is immediately after this loop.
                        // eslint-disable-next-line no-restricted-syntax
                        return true
                    }
                }
            }
        }
        return false
    }

    /** Regenerate {@link ShortcutRegistry.keyboardShortcutsByKey}. */
    updateKeyboardShortcutsByKey() {
        this.keyboardShortcutsByKey = {}
        for (const shortcuts of Object.values(this.keyboardShortcuts)) {
            for (const shortcut of shortcuts) {
                const byKey = this.keyboardShortcutsByKey[shortcut.key.toUpperCase()]
                if (byKey != null) {
                    byKey.unshift(shortcut)
                } else {
                    this.keyboardShortcutsByKey[shortcut.key.toUpperCase()] = [shortcut]
                }
            }
        }
    }

    /** Regenerate {@link ShortcutRegistry.activeKeyboardHandlers}. */
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
            Record<KeyboardAction, (event: KeyboardEvent | React.KeyboardEvent) => void>
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
    button: MouseButton
): MouseShortcut {
    return {
        button,
        action,
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
const DEFAULT_KEYBOARD_SHORTCUTS: Record<KeyboardAction, KeyboardShortcut[]> = {
    [KeyboardAction.open]: [keybind(KeyboardAction.open, [], 'Enter')],
    [KeyboardAction.close]: [],
    [KeyboardAction.uploadToCloud]: [],
    [KeyboardAction.rename]: [keybind(KeyboardAction.rename, [CTRL], 'R')],
    [KeyboardAction.snapshot]: [keybind(KeyboardAction.snapshot, [CTRL], 'S')],
    [KeyboardAction.moveToTrash]: [keybind(KeyboardAction.moveToTrash, [], DELETE)],
    [KeyboardAction.moveAllToTrash]: [keybind(KeyboardAction.moveAllToTrash, [], DELETE)],
    [KeyboardAction.share]: [keybind(KeyboardAction.share, [CTRL], 'Enter')],
    [KeyboardAction.label]: [keybind(KeyboardAction.label, [CTRL], 'L')],
    [KeyboardAction.duplicate]: [keybind(KeyboardAction.duplicate, [CTRL], 'D')],
    [KeyboardAction.copy]: [keybind(KeyboardAction.copy, [CTRL], 'C')],
    [KeyboardAction.cut]: [keybind(KeyboardAction.cut, [CTRL], 'X')],
    [KeyboardAction.download]: [keybind(KeyboardAction.download, [CTRL, 'Shift'], 'S')],
    [KeyboardAction.uploadFiles]: [keybind(KeyboardAction.uploadFiles, [CTRL], 'U')],
    [KeyboardAction.newProject]: [keybind(KeyboardAction.newProject, [CTRL], 'N')],
    [KeyboardAction.newFolder]: [keybind(KeyboardAction.newFolder, [CTRL, 'Shift'], 'N')],
    [KeyboardAction.newDataConnector]: [
        keybind(KeyboardAction.newDataConnector, [CTRL, 'Alt'], 'N'),
    ],
    [KeyboardAction.closeModal]: [keybind(KeyboardAction.closeModal, [], 'Escape')],
    [KeyboardAction.cancelEditName]: [keybind(KeyboardAction.cancelEditName, [], 'Escape')],
    [KeyboardAction.changeYourPassword]: [],
    [KeyboardAction.signIn]: [],
    [KeyboardAction.signOut]: [],
}

/** The default UI data for every keyboard shortcut. */
const DEFAULT_KEYBOARD_SHORTCUT_INFO: Record<KeyboardAction, ShortcutInfo> = {
    [KeyboardAction.open]: { name: 'Open', icon: OpenIcon },
    [KeyboardAction.close]: { name: 'Close', icon: CloseIcon },
    [KeyboardAction.uploadToCloud]: { name: 'Upload To Cloud', icon: CloudToIcon },
    [KeyboardAction.rename]: { name: 'Rename', icon: PenIcon },
    [KeyboardAction.snapshot]: { name: 'Snapshot', icon: CameraIcon },
    [KeyboardAction.moveToTrash]: {
        name: 'Move To Trash',
        icon: TrashIcon,
        colorClass: 'text-delete',
    },
    [KeyboardAction.moveAllToTrash]: {
        name: 'Move All To Trash',
        icon: TrashIcon,
        colorClass: 'text-delete',
    },
    [KeyboardAction.share]: { name: 'Share', icon: PeopleIcon },
    [KeyboardAction.label]: { name: 'Label', icon: TagIcon },
    [KeyboardAction.duplicate]: { name: 'Duplicate', icon: DuplicateIcon },
    [KeyboardAction.copy]: { name: 'Copy', icon: CopyIcon },
    [KeyboardAction.cut]: { name: 'Cut', icon: ScissorsIcon },
    [KeyboardAction.download]: { name: 'Download', icon: DataDownloadIcon },
    [KeyboardAction.uploadFiles]: { name: 'Upload Files', icon: DataUploadIcon },
    [KeyboardAction.newProject]: { name: 'New Project', icon: AddNetworkIcon },
    [KeyboardAction.newFolder]: { name: 'New Folder', icon: AddFolderIcon },
    [KeyboardAction.newDataConnector]: { name: 'New Data Connector', icon: AddConnectorIcon },
    // These should not appear in any context menus.
    [KeyboardAction.closeModal]: { name: 'Close', icon: BlankIcon },
    [KeyboardAction.cancelEditName]: { name: 'Cancel Editing', icon: BlankIcon },
    [KeyboardAction.changeYourPassword]: { name: 'Change Your Password', icon: ChangePasswordIcon },
    [KeyboardAction.signIn]: { name: 'Sign In', icon: SignInIcon },
    [KeyboardAction.signOut]: { name: 'Sign Out', icon: SignOutIcon, colorClass: 'text-delete' },
}

/** The default mouse shortcuts. */
const DEFAULT_MOUSE_SHORTCUTS: Record<MouseAction, MouseShortcut[]> = {
    [MouseAction.editName]: [mousebind(MouseAction.editName, [CTRL], MouseButton.left)],
    [MouseAction.selectAdditional]: [
        mousebind(MouseAction.selectAdditional, [CTRL], MouseButton.left),
    ],
    [MouseAction.selectRange]: [mousebind(MouseAction.selectRange, ['Shift'], MouseButton.left)],
    [MouseAction.selectAdditionalRange]: [
        mousebind(MouseAction.selectAdditionalRange, [CTRL, 'Shift'], MouseButton.left),
    ],
}
