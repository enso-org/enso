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
import Play2Icon from 'enso-assets/play2.svg'
import ScissorsIcon from 'enso-assets/scissors.svg'
import SignInIcon from 'enso-assets/sign_in.svg'
import SignOutIcon from 'enso-assets/sign_out.svg'
import TagIcon from 'enso-assets/tag.svg'
import TrashIcon from 'enso-assets/trash.svg'

import * as detect from 'enso-common/src/detect'

// This file MUST be a `.tsx` file so that Tailwind includes the CSS classes used here.

/* eslint-disable @typescript-eslint/naming-convention */

// =================
// === Constants ===
// =================

/** The size (both width and height) of icons. */
export const ICON_SIZE_PX = 16

// =============
// === Types ===
// =============

/** All possible mouse actions for which shortcuts can be registered. */
// This MUST be `as const`, because a type is derived from this constant.
// eslint-disable-next-line no-restricted-syntax
export const MOUSE_ACTIONS = [
    'open',
    /** Run without opening the editor. */
    'run',
    'edit-name',
    'select-additional',
    'select-range',
    'select-additional-range',
] as const

/** All mouse actions registered by the dashboard. */
type DashboardMouseAction = (typeof MOUSE_ACTIONS)[number]
/** Merge extra keys into this type to add new mouse actions. */
export interface MouseActions extends Record<DashboardMouseAction, true> {}
/** All mouse actions registered anywhere. */
export type MouseAction = keyof MouseActions

/** All possible keyboard actions for which shortcuts can be registered. */
// This MUST be `as const`, because a type is derived from this constant.
// eslint-disable-next-line no-restricted-syntax
export const KEYBOARD_ACTIONS = [
    'open',
    /** Run without opening the editor. */
    'run',
    'close',
    'upload-to-cloud',
    'rename',
    'snapshot',
    'move-to-trash',
    'move-all-to-trash',
    'delete',
    'delete-all',
    'share',
    'label',
    'duplicate',
    'copy',
    'cut',
    'download',
    'upload-files',
    'new-project',
    'new-folder',
    'new-data-connector',
    'close-modal',
    'cancel-edit-name',
    'change-your-password',
    'sign-in',
    'sign-out',
] as const

/** All keyboard actions registered by the dashboard. */
type DashboardKeyboardAction = (typeof KEYBOARD_ACTIONS)[number]
/** Merge extra keys into this type to add new keyboard actions. */
export interface KeyboardActions extends Record<DashboardKeyboardAction, true> {}
/** All keyboard actions registered anywhere. */
export type KeyboardAction = keyof KeyboardActions

/** Valid mouse buttons. The values of each enum member is its corresponding value of
 * `MouseEvent.button`. */
export enum MouseButton {
    move = -1,
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
    clicks: number
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
        open: make(),
        run: make(),
        close: make(),
        'upload-to-cloud': make(),
        rename: make(),
        snapshot: make(),
        'move-to-trash': make(),
        'move-all-to-trash': make(),
        delete: make(),
        'delete-all': make(),
        share: make(),
        label: make(),
        duplicate: make(),
        copy: make(),
        cut: make(),
        download: make(),
        'upload-files': make(),
        'new-project': make(),
        'new-folder': make(),
        'new-data-connector': make(),
        'close-modal': make(),
        'cancel-edit-name': make(),
        'change-your-password': make(),
        'sign-in': make(),
        'sign-out': make(),
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
        // eslint-disable-next-line @typescript-eslint/no-invalid-void-type
        ((event: KeyboardEvent | React.KeyboardEvent) => boolean | void)[]
    > = makeKeyboardActionMap(() => [])
    /** The last handler (if any) for each action in
     * {@link ShortcutRegistry.allKeyboardHandlers}. */
    activeKeyboardHandlers: Record<
        KeyboardAction,
        // eslint-disable-next-line @typescript-eslint/no-invalid-void-type
        ((event: KeyboardEvent | React.KeyboardEvent) => boolean | void) | null
    > = makeKeyboardActionMap(() => null)

    /** Create a {@link ShortcutRegistry}. */
    constructor(
        public keyboardActions: KeyboardAction[],
        public mouseActions: MouseAction[],
        public keyboardShortcuts: Record<KeyboardAction, KeyboardShortcut[]>,
        public mouseShortcuts: Record<MouseAction, MouseShortcut[]>,
        public keyboardShortcutInfo: Record<KeyboardAction, ShortcutInfo>
    ) {
        this.updateKeyboardShortcutsByKey()
    }

    /** Create a new {@link ShortcutRegistry} with default values. */
    static createWithDefaults() {
        return new this(
            [...KEYBOARD_ACTIONS],
            [...MOUSE_ACTIONS],
            { ...DEFAULT_KEYBOARD_SHORTCUTS },
            { ...DEFAULT_MOUSE_SHORTCUTS },
            { ...DEFAULT_KEYBOARD_SHORTCUT_INFO }
        )
    }

    /** Add new keyboard actions, and their corresponding shortcuts. */
    registerNewKeyboardActions<NewActions extends KeyboardAction>(
        newActions: readonly NewActions[],
        newShortcuts: Record<NewActions, KeyboardShortcut[]>
    ) {
        this.keyboardActions.push(...newActions)
        Object.assign(this.keyboardShortcuts, newShortcuts)
        for (const action of newActions) {
            this.allKeyboardHandlers[action] = []
        }
        this.updateKeyboardShortcutsByKey()
    }

    /** Add new mouse actions, and their corresponding shortcuts. */
    registerNewMouseActions<NewActions extends MouseAction>(
        newActions: readonly NewActions[],
        newShortcuts: Record<NewActions, MouseShortcut[]>
    ) {
        this.mouseActions.push(...newActions)
        Object.assign(this.mouseShortcuts, newShortcuts)
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
        const button: number = shortcut.button
        return (
            button === event.button &&
            Math.max(event.detail, 1) >= shortcut.clicks &&
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
        for (const action of this.keyboardActions) {
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
        for (const action of this.keyboardActions) {
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
export function keybind(
    action: KeyboardAction,
    modifiers: ModifierKey[],
    key: string
): KeyboardShortcut {
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
export function mousebind(
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
export const CTRL: 'Ctrl' | 'Meta' = (detect.isOnMacOS() ? 'Meta' : 'Ctrl') satisfies ModifierKey

/** The key known as the `Delete` key for the current platform. */
export const DELETE = detect.isOnMacOS() ? 'Backspace' : 'Delete'

/** The default keyboard shortcuts. */
const DEFAULT_KEYBOARD_SHORTCUTS: Record<KeyboardAction, KeyboardShortcut[]> = {
    open: [keybind('open', [], 'Enter')],
    run: [keybind('run', ['Shift'], 'Enter')],
    close: [],
    'upload-to-cloud': [],
    rename: [keybind('rename', [CTRL], 'R')],
    snapshot: [keybind('snapshot', [CTRL], 'S')],
    'move-to-trash': [keybind('move-to-trash', [], DELETE)],
    'move-all-to-trash': [keybind('move-all-to-trash', [], DELETE)],
    delete: [keybind('delete', [], DELETE)],
    'delete-all': [keybind('delete-all', [], DELETE)],
    share: [keybind('share', [CTRL], 'Enter')],
    label: [keybind('label', [CTRL], 'L')],
    duplicate: [keybind('duplicate', [CTRL], 'D')],
    copy: [keybind('copy', [CTRL], 'C')],
    cut: [keybind('cut', [CTRL], 'X')],
    download: [keybind('download', [CTRL, 'Shift'], 'S')],
    'upload-files': [keybind('upload-files', [CTRL], 'U')],
    'new-project': [keybind('new-project', [CTRL], 'N')],
    'new-folder': [keybind('new-folder', [CTRL, 'Shift'], 'N')],
    'new-data-connector': [keybind('new-data-connector', [CTRL, 'Alt'], 'N')],
    'close-modal': [keybind('close-modal', [], 'Escape')],
    'cancel-edit-name': [keybind('cancel-edit-name', [], 'Escape')],
    'change-your-password': [],
    'sign-in': [],
    'sign-out': [],
}

/** The default UI data for every keyboard shortcut. */
const DEFAULT_KEYBOARD_SHORTCUT_INFO: Record<KeyboardAction, ShortcutInfo> = {
    open: { name: 'Open', icon: OpenIcon },
    run: { name: 'Run', icon: Play2Icon },
    close: { name: 'Close', icon: CloseIcon },
    'upload-to-cloud': { name: 'Upload To Cloud', icon: CloudToIcon },
    rename: { name: 'Rename', icon: PenIcon },
    snapshot: { name: 'Snapshot', icon: CameraIcon },
    'move-to-trash': {
        name: 'Move To Trash',
        icon: TrashIcon,
        colorClass: 'text-delete',
    },
    'move-all-to-trash': {
        name: 'Move All To Trash',
        icon: TrashIcon,
        colorClass: 'text-delete',
    },
    delete: { name: 'Delete', icon: TrashIcon, colorClass: 'text-delete' },
    'delete-all': { name: 'Delete All', icon: TrashIcon, colorClass: 'text-delete' },
    share: { name: 'Share', icon: PeopleIcon },
    label: { name: 'Label', icon: TagIcon },
    duplicate: { name: 'Duplicate', icon: DuplicateIcon },
    copy: { name: 'Copy', icon: CopyIcon },
    cut: { name: 'Cut', icon: ScissorsIcon },
    download: { name: 'Download', icon: DataDownloadIcon },
    'upload-files': { name: 'Upload Files', icon: DataUploadIcon },
    'new-project': { name: 'New Project', icon: AddNetworkIcon },
    'new-folder': { name: 'New Folder', icon: AddFolderIcon },
    'new-data-connector': { name: 'New Data Connector', icon: AddConnectorIcon },
    // These should not appear in any context menus.
    'close-modal': { name: 'Close', icon: BlankIcon },
    'cancel-edit-name': { name: 'Cancel Editing', icon: BlankIcon },
    'change-your-password': { name: 'Change Your Password', icon: ChangePasswordIcon },
    'sign-in': { name: 'Sign In', icon: SignInIcon },
    'sign-out': { name: 'Sign Out', icon: SignOutIcon, colorClass: 'text-delete' },
}

/** The default mouse shortcuts. */
const DEFAULT_MOUSE_SHORTCUTS: Record<MouseAction, MouseShortcut[]> = {
    open: [mousebind('open', [], MouseButton.left, 2)],
    run: [mousebind('run', ['Shift'], MouseButton.left, 2)],
    'edit-name': [mousebind('edit-name', [CTRL], MouseButton.left, 1)],
    'select-additional': [mousebind('select-additional', [CTRL], MouseButton.left, 1)],
    'select-range': [mousebind('select-range', ['Shift'], MouseButton.left, 1)],
    'select-additional-range': [
        mousebind('select-additional-range', [CTRL, 'Shift'], MouseButton.left, 1),
    ],
}
