/** @file A registry for keyboard and mouse shortcuts. */
import * as React from 'react'

import * as detect from 'enso-common/src/detect'

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
export type ModifierKey = 'Alt' | 'Ctrl' | 'Meta' | 'Shift'

// ===========================
// === modifiersMatchEvent ===
// ===========================

/** Return `true` if and only if the modifiers match the evenet's modifier key states. */
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
        public mouseShortcuts: Record<MouseAction, MouseShortcut[]>
    ) {}

    /** Return `true` if the specified action is being triggered by the given event. */
    matchesKeyboardAction(action: KeyboardAction, event: KeyboardEvent | React.KeyboardEvent) {
        return this.keyboardShortcuts[action].some(
            shortcut => shortcut.key === event.key && modifiersMatchEvent(shortcut, event)
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
const CTRL = detect.platform() === detect.Platform.macOS ? 'Meta' : 'Ctrl'

/** The default keyboard shortcuts. */
const DEFAULT_KEYBOARD_SHORTCUTS: Record<KeyboardAction, KeyboardShortcut[]> = {
    [KeyboardAction.closeModal]: [keybind([], 'Escape')],
    [KeyboardAction.cancelEditName]: [keybind([], 'Escape')],
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
    DEFAULT_MOUSE_SHORTCUTS
)
