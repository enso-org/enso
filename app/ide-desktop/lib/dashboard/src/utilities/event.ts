/** @file Utility functions related to event handling. */
import type * as React from 'react'

import * as detect from 'enso-common/src/detect'

// =============================
// === Mouse event utilities ===
// =============================

/** Returns `true` if and only if the event is a single click event. */
export function isSingleClick(event: React.MouseEvent) {
  return event.detail === 1
}

/** Returns `true` if and only if the event is a double click event. */
export function isDoubleClick(event: React.MouseEvent) {
  return event.detail === 2
}

/** Returns `true` if and only if the event has the modifier key set
 * (`Ctrl` on Windows/Linux; `Cmd` on macOS). */
export function isModKey(event: React.KeyboardEvent | React.MouseEvent) {
  return detect.isOnMacOS() ? event.metaKey : event.ctrlKey
}

// ================================
// === Keyboard event utilities ===
// ================================

/** A {@link RegExp} that matches {@link KeyboardEvent.code}s corresponding to non-printable
 * keys. */
const SPECIAL_CHARACTER_KEYCODE_REGEX = /^[A-Z][a-z]/

/** Whether `event` may trigger a shortcut. */
export function isPotentiallyShortcut(event: KeyboardEvent | React.KeyboardEvent) {
  return event.ctrlKey || event.metaKey || event.altKey
}

/** Whether `event.key` is a key used in text editing. */
export function isTextInputKey(event: KeyboardEvent | React.KeyboardEvent) {
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

/** Whether the element accepts text input. */
export function isElementTextInput(element: EventTarget | null) {
  return (
    element != null &&
    (element instanceof HTMLInputElement ||
      element instanceof HTMLTextAreaElement ||
      (element instanceof HTMLElement && element.isContentEditable))
  )
}
