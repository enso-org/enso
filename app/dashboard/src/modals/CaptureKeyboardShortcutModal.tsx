/** @file A modal for capturing an arbitrary keyboard shortcut. */
import * as React from 'react'

import * as detect from 'enso-common/src/detect'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import KeyboardShortcut from '#/components/dashboard/KeyboardShortcut'
import Modal from '#/components/Modal'

import * as inputBindings from '#/utilities/inputBindings'
import * as tailwindMerge from '#/utilities/tailwindMerge'

// ==============================
// === eventToPartialShortcut ===
// ==============================

const DISALLOWED_KEYS = new Set(['Control', 'Alt', 'Shift', 'Meta'])
const DELETE_KEY = detect.isOnMacOS() ? 'Backspace' : 'Delete'

/** Extracts a partial keyboard shortcut from a {@link KeyboardEvent}. */
function eventToPartialShortcut(event: KeyboardEvent | React.KeyboardEvent) {
  const modifiers = inputBindings
    .modifiersForModifierFlags(inputBindings.modifierFlagsForEvent(event))
    .join('+')
  // `Tab` and `Shift+Tab` should be reserved for keyboard navigation
  const key =
    (
      DISALLOWED_KEYS.has(event.key) ||
      (!event.ctrlKey && !event.altKey && !event.metaKey && event.key === 'Tab')
    ) ?
      null
    : event.key === ' ' ? 'Space'
    : event.key === DELETE_KEY ? 'OsDelete'
    : inputBindings.normalizedKeyboardSegmentLookup[event.key.toLowerCase()] ?? event.key
  return { key, modifiers }
}

// ====================================
// === CaptureKeyboardShortcutModal ===
// ====================================

/** Props for a {@link CaptureKeyboardShortcutModal}. */
export interface CaptureKeyboardShortcutModalProps {
  readonly description: string
  readonly existingShortcuts: Set<string>
  readonly onSubmit: (shortcut: string) => void
}

/** A modal for capturing an arbitrary keyboard shortcut. */
export default function CaptureKeyboardShortcutModal(props: CaptureKeyboardShortcutModalProps) {
  const { description, existingShortcuts, onSubmit } = props
  const { unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const [key, setKey] = React.useState<string | null>(null)
  const [modifiers, setModifiers] = React.useState<string>('')
  const shortcut =
    key == null ? modifiers
    : modifiers === '' ? key
    : `${modifiers}+${key}`
  const doesAlreadyExist = key != null && existingShortcuts.has(shortcut)
  const canSubmit = key != null && !doesAlreadyExist

  return (
    <Modal centered className="bg-dim">
      <form
        ref={(element) => {
          element?.focus()
        }}
        tabIndex={-1}
        className="pointer-events-auto relative flex w-capture-keyboard-shortcut-modal flex-col items-center gap-modal rounded-default p-modal before:absolute before:inset before:h-full before:w-full before:rounded-default before:bg-selected-frame before:backdrop-blur-3xl"
        onKeyDown={(event) => {
          if (event.key === 'Escape' && key === 'Escape') {
            // Ignore.
          } else if (event.key === 'Enter' && key != null) {
            event.currentTarget.requestSubmit()
          } else {
            event.preventDefault()
            event.stopPropagation()
            const newShortcut = eventToPartialShortcut(event)
            if (event.key === 'Tab' && newShortcut.key == null) {
              // Ignore.
            } else {
              setKey(newShortcut.key)
              setModifiers(newShortcut.modifiers)
            }
          }
        }}
        onKeyUp={(event) => {
          if (key == null) {
            // A modifier may have been released.
            const newShortcut = eventToPartialShortcut(event)
            setModifiers(newShortcut.modifiers)
          }
        }}
        onClick={(event) => {
          event.stopPropagation()
        }}
        onSubmit={(event) => {
          event.preventDefault()
          if (canSubmit) {
            unsetModal()
            onSubmit(shortcut)
          }
        }}
      >
        <div className="relative">{getText('enterTheNewKeyboardShortcutFor', description)}</div>
        <div
          className={tailwindMerge.twMerge(
            'relative flex scale-150 items-center justify-center',
            doesAlreadyExist && 'text-red-600',
          )}
        >
          {shortcut === '' ?
            <aria.Text className="text text-primary/30">{getText('noShortcutEntered')}</aria.Text>
          : <KeyboardShortcut shortcut={shortcut} />}
        </div>
        <aria.Text className="relative text-red-600">
          {doesAlreadyExist ? 'This shortcut already exists.' : ''}
        </aria.Text>
        <ariaComponents.ButtonGroup>
          <ariaComponents.Button
            variant="submit"
            isDisabled={!canSubmit}
            onPress={() => {
              unsetModal()
              onSubmit(shortcut)
            }}
          >
            {getText('confirm')}
          </ariaComponents.Button>
          <ariaComponents.Button variant="outline" onPress={unsetModal}>
            {getText('cancel')}
          </ariaComponents.Button>
        </ariaComponents.ButtonGroup>
      </form>
    </Modal>
  )
}
