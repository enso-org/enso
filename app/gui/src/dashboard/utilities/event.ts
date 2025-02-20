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

/**
 * Returns `true` if and only if the event has the modifier key set
 * (`Ctrl` on Windows/Linux; `Cmd` on macOS).
 */
export function isModKey(event: React.KeyboardEvent | React.MouseEvent) {
  return detect.isOnMacOS() ? event.metaKey : event.ctrlKey
}

// ================================
// === Keyboard event utilities ===
// ================================

/**
 * A {@link RegExp} that matches {@link KeyboardEvent.code}s corresponding to non-printable
 * keys.
 */
const SPECIAL_CHARACTER_KEYCODE_REGEX = /^[A-Z][a-z]/

/** Whether `event` may trigger a shortcut. */
export function isPotentiallyShortcut(event: KeyboardEvent | React.KeyboardEvent) {
  return event.ctrlKey || event.metaKey || event.altKey
}

/** Return `true` if none of `Ctrl`, `Shift`, `Alt` and `Meta` are pressed.*/
export function areNoModifiersPressed(
  event: KeyboardEvent | MouseEvent | React.KeyboardEvent | React.MouseEvent,
) {
  return !event.ctrlKey && !event.metaKey && !event.shiftKey && !event.altKey
}

/** Whether `event.key` is an arrow key. */
export function isArrowKeyEvent(event: KeyboardEvent | React.KeyboardEvent) {
  return (
    event.key === 'ArrowLeft' ||
    event.key === 'ArrowRight' ||
    event.key === 'ArrowUp' ||
    event.key === 'ArrowDown'
  )
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
export function isElementTextInput(
  element: EventTarget | null,
): element is HTMLElement | HTMLInputElement | HTMLTextAreaElement {
  return (
    element != null &&
    (isElementSingleLineTextInput(element) ||
      element instanceof HTMLTextAreaElement ||
      (element instanceof HTMLElement && element.isContentEditable))
  )
}

const TEXT_INPUT_TYPES = new Set([
  'text',
  'password',
  'search',
  'tel',
  'number',
  'email',
  'month',
  'url',
  'week',
  'datetime',
])

/** Whether the element is a single-line text input. */
export function isElementSingleLineTextInput(
  element: EventTarget | null,
): element is HTMLInputElement {
  return (
    element != null && element instanceof HTMLInputElement && TEXT_INPUT_TYPES.has(element.type)
  )
}

// =============================
// === isElementPartOfMonaco ===
// =============================

/** Whether the element is part of a Monaco editor. */
export function isElementPartOfMonaco(element: EventTarget | null) {
  const recursiveCheck = (htmlElement: HTMLElement | null): boolean => {
    if (htmlElement == null || htmlElement === document.body) {
      return false
    } else if (
      htmlElement instanceof HTMLElement &&
      htmlElement.classList.contains('monaco-editor')
    ) {
      return true
    } else {
      return recursiveCheck(htmlElement.parentElement)
    }
  }
  return element != null && element instanceof HTMLElement && recursiveCheck(element)
}

// =========================
// === isElementInBounds ===
// =========================

/** Whether the event occurred within the given {@link DOMRect}. */
export function isElementInBounds(
  event: Pick<MouseEvent, 'clientX' | 'clientY'>,
  bounds: DOMRect,
  margin = 0,
) {
  return (
    bounds.left - margin <= event.clientX &&
    event.clientX <= bounds.right + margin &&
    bounds.top - margin <= event.clientY &&
    event.clientY <= bounds.bottom + margin
  )
}

// ==================
// === submitForm ===
// ==================

/** An event with an {@link Element} as its target. */
interface EventWithElementTarget {
  readonly target: Element
}

/** Search for an ancestor `form` element and try to submit it. */
export function submitForm(event: EventWithElementTarget) {
  const closestForm = event.target.closest('form')
  if (closestForm != null) {
    closestForm.requestSubmit()
  }
}
