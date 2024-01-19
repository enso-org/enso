/** @file Tests for `dateTime.ts`. */
import * as v from 'vitest'

import ShortcutManager, * as shortcutManager from '../ShortcutManager'

// =============
// === Tests ===
// =============

// NOTE: macOS using the meta key has not been tested, as it is not possible to override
// `navigator.userAgent`.

v.test.each([
    {
        event: new KeyboardEvent('keydown', { key: 'N', ctrlKey: true }),
        action: shortcutManager.KeyboardAction.newProject,
    },
    {
        event: new KeyboardEvent('keydown', { key: 'N', ctrlKey: true, shiftKey: true }),
        action: shortcutManager.KeyboardAction.newFolder,
    },
    {
        event: new KeyboardEvent('keydown', { key: 'N', ctrlKey: true, altKey: true }),
        action: shortcutManager.KeyboardAction.newDataConnector,
    },
    {
        event: new KeyboardEvent('keydown', { key: 'C', ctrlKey: true }),
        action: shortcutManager.KeyboardAction.copy,
    },
    {
        event: new KeyboardEvent('keydown', { key: 'X', ctrlKey: true }),
        action: shortcutManager.KeyboardAction.cutAll,
    },
    {
        event: new KeyboardEvent('keydown', { key: 'V', ctrlKey: true }),
        action: shortcutManager.KeyboardAction.paste,
    },
    // Disallow extra modifier keys
    {
        event: new KeyboardEvent('keydown', { key: 'N', ctrlKey: true, metaKey: true }),
        action: null,
    },
    // Disallow invalid shortcutManager
    {
        event: new KeyboardEvent('keydown', { key: 'J' }),
        action: null,
    },
    {
        event: new KeyboardEvent('keydown', { key: 'J', ctrlKey: true }),
        action: null,
    },
])('Keyboard shortcut handling', ({ event, action }) => {
    const shortcutManager = ShortcutManager.createWithDefaults()
    const matchedAction = v.vi.fn((shortcut: shortcutManager.KeyboardAction) => shortcut)
    shortcutManager.registerKeyboardHandlers(
        new Proxy(
            {},
            {
                /** Return a function that logs the key. */
                get(_, key) {
                    // This is SAFE, as all keys are known at typecheck time.
                    // eslint-disable-next-line no-restricted-syntax
                    return () => matchedAction(key as shortcutManager.KeyboardAction)
                },
            }
        )
    )
    const eventString =
        (event.ctrlKey ? 'Ctrl+' : '') +
        (event.shiftKey ? 'Shift+' : '') +
        (event.altKey ? 'Alt+' : '') +
        (event.metaKey ? 'Cmd+' : '') +
        event.key
    if (action != null) {
        v.expect(
            shortcutManager.handleKeyboardEvent(event),
            `'${eventString}' should match a keyboard action`
        ).toBe(true)
        v.expect(
            matchedAction,
            `'${eventString}' should match the keyboard action '${action}'`
        ).toBeCalledWith(action)
    } else {
        v.expect(
            shortcutManager.handleKeyboardEvent(event),
            `'${eventString}' should not match any keyboard action`
        ).toBe(false)
    }
})

v.test.each([
    // === Should match ===
    {
        event: new MouseEvent('mousedown', { detail: 2 }),
        action: shortcutManager.MouseAction.open,
    },
    {
        event: new MouseEvent('mousedown', { ctrlKey: true, detail: 1 }),
        action: shortcutManager.MouseAction.selectAdditional,
    },
    {
        event: new MouseEvent('mousedown', { ctrlKey: true, shiftKey: true, detail: 1 }),
        action: shortcutManager.MouseAction.selectAdditionalRange,
    },
    {
        event: new MouseEvent('mousedown', { shiftKey: true, detail: 1 }),
        action: shortcutManager.MouseAction.selectRange,
    },
    {
        event: new MouseEvent('mousedown', { ctrlKey: true, detail: 1 }),
        action: shortcutManager.MouseAction.editName,
    },
    // Triple click or double click instead of single click SHOULD match
    {
        event: new MouseEvent('mousedown', { ctrlKey: true, detail: 3 }),
        action: shortcutManager.MouseAction.selectAdditional,
    },
    {
        event: new MouseEvent('mousedown', { ctrlKey: true, detail: 2 }),
        action: shortcutManager.MouseAction.selectAdditional,
    },
    // Triple click instead of double click SHOULD match
    {
        event: new MouseEvent('mousedown', { detail: 3 }),
        action: shortcutManager.MouseAction.open,
    },
    // === Should not match ===
    // Single click instad of double click
    {
        event: new MouseEvent('mousedown', { detail: 1 }),
        action: shortcutManager.MouseAction.open,
        match: false,
    },
    // 0 clicks instad of double click
    {
        event: new MouseEvent('mousedown', { detail: 0 }),
        action: shortcutManager.MouseAction.open,
        match: false,
    },
    {
        event: new MouseEvent('mousedown'),
        action: shortcutManager.MouseAction.open,
        match: false,
    },
    // Missing modifier keys
    {
        event: new MouseEvent('mousedown', { ctrlKey: true, detail: 2 }),
        action: shortcutManager.MouseAction.selectAdditionalRange,
        match: false,
    },
    // Extra modifier keys
    {
        event: new MouseEvent('mousedown', {
            ctrlKey: true,
            altKey: true,
            metaKey: true,
            detail: 2,
        }),
        action: shortcutManager.MouseAction.selectAdditional,
        match: false,
    },
    {
        event: new MouseEvent('mousedown', { ctrlKey: true }),
        action: shortcutManager.MouseAction.open,
        match: false,
    },
])('Mouse shortcut handling', ({ event, action, match = true }) => {
    const shortcutManager = ShortcutManager.createWithDefaults()
    v.expect(
        shortcutManager.matchesMouseAction(action, event),
        `'${
            (event.ctrlKey ? 'Ctrl+' : '') +
            (event.shiftKey ? 'Shift+' : '') +
            (event.altKey ? 'Alt+' : '') +
            (event.metaKey ? 'Cmd+' : '') +
            (event.detail === 0
                ? 'No Click'
                : event.detail === 1
                ? 'Click'
                : event.detail === 2
                ? 'Double Click'
                : 'Triple Click')
        }' should${match ? '' : ' not'} match the mouse action '${action}'`
    ).toBe(match)
})
