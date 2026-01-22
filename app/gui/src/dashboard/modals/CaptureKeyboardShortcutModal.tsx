/** @file A modal for capturing an arbitrary keyboard shortcut. */
import { Button } from '#/components/Button'
import { Dialog } from '#/components/Dialog'
import { Form } from '#/components/Form'
import { Text } from '#/components/Text'
import KeyboardShortcut from '#/pages/dashboard/components/KeyboardShortcut'
import { unsetModal } from '#/providers/ModalProvider'
import { twMerge } from '#/utilities/tailwindMerge'
import { useText } from '$/providers/react'
import {
  modifierFlagsForEvent,
  modifiersForModifierFlags,
  normalizedKeyboardSegmentLookup,
} from '@/util/shortcuts'
import { isOnMacOS } from 'enso-common/src/utilities/detect'
import { useState, type KeyboardEvent as ReactKeyboardEvent } from 'react'

const DISALLOWED_KEYS = new Set(['Control', 'Alt', 'Shift', 'Meta'])
const DELETE_KEY = isOnMacOS() ? 'Backspace' : 'Delete'

/** Extracts a partial keyboard shortcut from a {@link KeyboardEvent}. */
function eventToPartialShortcut(event: KeyboardEvent | ReactKeyboardEvent) {
  const modifiers = modifiersForModifierFlags(modifierFlagsForEvent(event)).join('+')
  // `Tab` and `Shift+Tab` should be reserved for keyboard navigation
  const key =
    (
      DISALLOWED_KEYS.has(event.key) ||
      (!event.ctrlKey && !event.altKey && !event.metaKey && event.key === 'Tab')
    ) ?
      null
    : event.key === ' ' ? 'Space'
    : event.key === DELETE_KEY ? 'OsDelete'
    : (normalizedKeyboardSegmentLookup[event.key.toLowerCase()] ?? event.key)
  return { key, modifiers }
}

/** Props for a {@link CaptureKeyboardShortcutModal}. */
export interface CaptureKeyboardShortcutModalProps {
  readonly description: string
  readonly existingShortcuts: Set<string>
  readonly onSubmit: (shortcut: string) => void
}

/** A modal for capturing an arbitrary keyboard shortcut. */
export default function CaptureKeyboardShortcutModal(props: CaptureKeyboardShortcutModalProps) {
  const { description, existingShortcuts, onSubmit } = props
  const { getText } = useText()
  const [key, setKey] = useState<string | null>(null)
  const [modifiers, setModifiers] = useState<string>('')
  const shortcut =
    key == null ? modifiers
    : modifiers === '' ? key
    : `${modifiers}+${key}`
  const doesAlreadyExist = key != null && existingShortcuts.has(shortcut)
  const canSubmit = key != null && !doesAlreadyExist

  return (
    <Dialog>
      <Form
        ref={(element) => {
          element?.focus()
        }}
        tabIndex={-1}
        method="dialog"
        schema={(z) => z.object({})}
        className="flex-col items-center"
        gap="none"
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
        onSubmit={() => {
          if (canSubmit) {
            unsetModal()
            onSubmit(shortcut)
          }
        }}
      >
        <div className="relative">{getText('enterTheNewKeyboardShortcutFor', description)}</div>
        <div
          className={twMerge(
            'relative flex scale-150 items-center justify-center',
            doesAlreadyExist && 'text-red-600',
          )}
        >
          {shortcut === '' ?
            <Text>{getText('noShortcutEntered')}</Text>
          : <KeyboardShortcut shortcut={shortcut} />}
        </div>
        <Text className="relative text-red-600">
          {doesAlreadyExist ? 'This shortcut already exists.' : ''}
        </Text>
        <Button.Group>
          <Form.Submit isDisabled={!canSubmit}>{getText('confirm')}</Form.Submit>
          <Dialog.Close variant="outline">{getText('cancel')}</Dialog.Close>
        </Button.Group>
      </Form>
    </Dialog>
  )
}
