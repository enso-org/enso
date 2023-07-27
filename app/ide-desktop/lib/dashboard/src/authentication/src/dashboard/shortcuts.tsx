/** @file A registry for keyboard and mouse shortcuts. */
import * as React from 'react'

import AddConnectorIcon from 'enso-assets/add_connector.svg'
import AddFolderIcon from 'enso-assets/add_folder.svg'
import AddNetworkIcon from 'enso-assets/add_network.svg'
import BlankIcon from 'enso-assets/blank_16.svg'
import CameraIcon from 'enso-assets/camera.svg'
import CopyIcon from 'enso-assets/copy.svg'
import DataDownloadIcon from 'enso-assets/data_download.svg'
import DataUploadIcon from 'enso-assets/data_upload.svg'
import DuplicateIcon from 'enso-assets/duplicate.svg'
import OpenIcon from 'enso-assets/open.svg'
import PenIcon from 'enso-assets/pen.svg'
import PeopleIcon from 'enso-assets/people.svg'
import ScissorsIcon from 'enso-assets/scissors.svg'
import TagIcon from 'enso-assets/tag.svg'
import TrashIcon from 'enso-assets/trash.svg'

import * as detect from 'enso-common/src/detect'

// This file MUST be a `.tsx` file so that Tailwind includes the CSS classes used here.

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
}

/** A mouse shortcut. If a key is omitted, that means its value does not matter. */
export interface MouseShortcut extends Modifiers {
    button: MouseButton
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
    return [
        ...(event.meta === true ? (['Meta'] satisfies ModifierKey[]) : []),
        ...(event.shift === true ? (['Shift'] satisfies ModifierKey[]) : []),
        ...(event.alt === true ? (['Alt'] satisfies ModifierKey[]) : []),
        ...(event.ctrl === true ? (['Ctrl'] satisfies ModifierKey[]) : []),
    ]
}

// ===========================
// === modifiersMatchEvent ===
// ===========================

/** Return `true` if and only if the modifiers match the event's modifier key states. */
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
    /** Create a {@link ShortcutRegistry}. */
    constructor(
        public keyboardShortcuts: Record<KeyboardAction, KeyboardShortcut[]>,
        public mouseShortcuts: Record<MouseAction, MouseShortcut[]>,
        public keyboardShorcutInfo: Record<KeyboardAction, ShortcutInfo>
    ) {}

    /** Return `true` if the specified action is being triggered by the given event. */
    matchesKeyboardAction(action: KeyboardAction, event: KeyboardEvent | React.KeyboardEvent) {
        return this.keyboardShortcuts[action].some(
            shortcut =>
                shortcut.key === event.key.toUpperCase() && modifiersMatchEvent(shortcut, event)
        )
    }

    /** Return `true` if the specified action is being triggered by the given event. */
    matchesMouseAction(action: MouseAction, event: MouseEvent | React.MouseEvent) {
        return this.mouseShortcuts[action].some(
            shortcut => shortcut.button === event.button && modifiersMatchEvent(shortcut, event)
        )
    }
}

/** A shorthand for creating a {@link KeyboardShortcut}. Should only be used in
 * {@link DEFAULT_KEYBOARD_SHORTCUTS}. */
function keybind(modifiers: ModifierKey[], key: string): KeyboardShortcut {
    return {
        key: key,
        ctrl: modifiers.includes('Ctrl'),
        alt: modifiers.includes('Alt'),
        shift: modifiers.includes('Shift'),
        meta: modifiers.includes('Meta'),
    }
}

/** A shorthand for creating a {@link MouseShortcut}. Should only be used in
 * {@link DEFAULT_MOUSE_SHORTCUTS}. */
function mousebind(modifiers: ModifierKey[], button: MouseButton): MouseShortcut {
    return {
        button: button,
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
const CTRL = (detect.platform() === detect.Platform.macOS ? 'Meta' : 'Ctrl') satisfies ModifierKey

/** The default keyboard shortcuts. */
const DEFAULT_KEYBOARD_SHORTCUTS: Record<KeyboardAction, KeyboardShortcut[]> = {
    [KeyboardAction.open]: [keybind([], 'Enter')],
    [KeyboardAction.rename]: [keybind([CTRL], 'R')],
    [KeyboardAction.snapshot]: [keybind([CTRL], 'S')],
    [KeyboardAction.moveToTrash]: [keybind([], 'Delete')],
    [KeyboardAction.moveAllToTrash]: [keybind([], 'Delete')],
    [KeyboardAction.share]: [keybind([CTRL], 'Enter')],
    [KeyboardAction.label]: [keybind([CTRL], 'L')],
    [KeyboardAction.duplicate]: [keybind([CTRL], 'D')],
    [KeyboardAction.copy]: [keybind([CTRL], 'C')],
    [KeyboardAction.cut]: [keybind([CTRL], 'X')],
    [KeyboardAction.download]: [keybind([CTRL, 'Shift'], 'S')],
    [KeyboardAction.uploadFiles]: [keybind([CTRL], 'U')],
    [KeyboardAction.newProject]: [keybind([CTRL], 'N')],
    [KeyboardAction.newFolder]: [keybind([CTRL, 'Shift'], 'N')],
    [KeyboardAction.newDataConnector]: [keybind([CTRL, 'Alt'], 'N')],
    [KeyboardAction.closeModal]: [keybind([], 'Escape')],
    [KeyboardAction.cancelEditName]: [keybind([], 'Escape')],
}

/** The default UI data for every keyboard shortcut. */
const DEFAULT_KEYBOARD_SHORTCUT_INFO: Record<KeyboardAction, ShortcutInfo> = {
    [KeyboardAction.open]: { name: 'Open', icon: OpenIcon },
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
}

/** The default mouse shortcuts. */
const DEFAULT_MOUSE_SHORTCUTS: Record<MouseAction, MouseShortcut[]> = {
    [MouseAction.editName]: [mousebind([CTRL], MouseButton.left)],
    [MouseAction.selectAdditional]: [mousebind([CTRL], MouseButton.left)],
    [MouseAction.selectRange]: [mousebind(['Shift'], MouseButton.left)],
    [MouseAction.selectAdditionalRange]: [mousebind([CTRL, 'Shift'], MouseButton.left)],
}

/** The global instance of the shortcut registry. */
export const SHORTCUT_REGISTRY = new ShortcutRegistry(
    DEFAULT_KEYBOARD_SHORTCUTS,
    DEFAULT_MOUSE_SHORTCUTS,
    DEFAULT_KEYBOARD_SHORTCUT_INFO
)
