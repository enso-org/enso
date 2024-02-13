/** @file Settings tab for editing keyboard shortcuts. */
import * as React from 'react'

import CrossIcon from 'enso-assets/cross.svg'
import Plus2Icon from 'enso-assets/plus2.svg'

import * as shortcutManagerProvider from '#/providers/ShortcutManagerProvider'

import KeyboardShortcut from '#/components/dashboard/keyboardShortcut'

import * as object from '#/utilities/object'
import * as shortcutManagerModule from '#/utilities/ShortcutManager'

// ============================
// === Global configuration ===
// ============================

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    readonly keyboardShortcuts: Partial<
      Readonly<
        Record<shortcutManagerModule.KeyboardAction, shortcutManagerModule.KeyboardShortcut[]>
      >
    >
  }
}

// ============
// === save ===
// ============

// =================
// === InfoEntry ===
// =================

/** Props for a transparent wrapper component. */
interface InternalTransparentWrapperProps {
  readonly children: React.ReactNode
}

/** A transparent wrapper component */
// This is a React component even though it does not contain JSX.
// eslint-disable-next-line no-restricted-syntax
function Name(props: InternalTransparentWrapperProps) {
  return props.children
}

/** A transparent wrapper component */
// This is a React component even though it does not contain JSX.
// eslint-disable-next-line no-restricted-syntax
function Value(props: InternalTransparentWrapperProps) {
  return props.children
}

/** Props for a {@link InfoEntry}. */
interface InternalInfoEntryProps {
  readonly children: [React.ReactNode, React.ReactNode]
}

/** Styled information display containing key and value. */
function InfoEntry(props: InternalInfoEntryProps) {
  const { children } = props
  const [name, value] = children
  return (
    <div className="flex gap-4.75">
      <span className="leading-5 w-36 h-8 py-1.25">{name}</span>
      <span className="group flex items-center gap-1 grow font-bold leading-5 h-8 py-1.25">
        {value}
      </span>
    </div>
  )
}

// ====================================
// === KeyboardShortcutsSettingsTab ===
// ====================================

/** Settings tab for viewing and editing account information. */
export default function KeyboardShortcutsSettingsTab() {
  const { shortcutManager } = shortcutManagerProvider.useShortcutManager()

  return (
    <div className="flex flex-col gap-2.5">
      <h3 className="font-bold text-xl h-9.5 py-0.5">Keyboard shortcuts</h3>
      <div className="flex flex-col">
        {object.unsafeEntries(shortcutManager.keyboardShortcuts).map(kv => {
          const [action, shortcuts] = kv
          const info = shortcutManagerModule.KEYBOARD_SHORTCUT_INFO[action]
          return (
            <InfoEntry key={action}>
              <Name>{info.name}</Name>
              <Value>
                {shortcuts.map((shortcut, i) => (
                  <div key={i} className="flex items-center gap-1">
                    <KeyboardShortcut shortcut={shortcut} />
                    <button
                      className="inline-flex invisible group-hover:visible"
                      onClick={() => {
                        shortcutManager.removeKeyboardShortcut(action, shortcut)
                        // TODO: remove from shortcut manager
                      }}
                    >
                      <img src={CrossIcon} />
                    </button>
                  </div>
                ))}
                <button
                  className="inline-flex invisible group-hover:visible"
                  onClick={() => {
                    // TODO: add new shortcut to action - after capturing it
                  }}
                >
                  <img className="w-4.5 h-4.5" src={Plus2Icon} />
                </button>
              </Value>
            </InfoEntry>
          )
        })}
      </div>
    </div>
  )
}
