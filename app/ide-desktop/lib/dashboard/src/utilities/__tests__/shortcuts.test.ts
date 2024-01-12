/** @file Tests for `dateTime.ts`. */
import * as v from 'vitest'

import * as shortcuts from '../shortcuts'

// =============
// === Tests ===
// =============

// NOTE: macOS using the meta key has not been tested, as it is not possible to override
// `navigator.userAgent`.

v.test.each([
    {
        event: new KeyboardEvent('keydown', { key: 'N', ctrlKey: true }),
        action: shortcuts.KeyboardAction.newProject,
    },
    {
        event: new KeyboardEvent('keydown', { key: 'N', ctrlKey: true, shiftKey: true }),
        action: shortcuts.KeyboardAction.newFolder,
    },
    {
        event: new KeyboardEvent('keydown', { key: 'N', ctrlKey: true, altKey: true }),
        action: shortcuts.KeyboardAction.newDataConnector,
    },
    {
        event: new KeyboardEvent('keydown', { key: 'C', ctrlKey: true }),
        action: shortcuts.KeyboardAction.copy,
    },
    {
        event: new KeyboardEvent('keydown', { key: 'X', ctrlKey: true }),
        action: shortcuts.KeyboardAction.cutAll,
    },
    {
        event: new KeyboardEvent('keydown', { key: 'V', ctrlKey: true }),
        action: shortcuts.KeyboardAction.paste,
    },
    // Disallow extra modifier keys
    {
        event: new KeyboardEvent('keydown', { key: 'N', ctrlKey: true, metaKey: true }),
        action: null,
    },
    // Disallow invalid shortcuts
    {
        event: new KeyboardEvent('keydown', { key: 'J' }),
        action: null,
    },
    {
        event: new KeyboardEvent('keydown', { key: 'J', ctrlKey: true }),
        action: null,
    },
])('Keyboard shortcut handling', ({ event, action }) => {
    const registry = shortcuts.ShortcutRegistry.createWithDefaults()
    const matchedAction = v.vi.fn((shortcut: shortcuts.KeyboardAction) => shortcut)
    registry.registerKeyboardHandlers(
        new Proxy(
            {},
            {
                /** Return a function that logs the key. */
                get(_, key) {
                    // This is SAFE, as all keys are known at typecheck time.
                    // eslint-disable-next-line no-restricted-syntax
                    return () => matchedAction(key as shortcuts.KeyboardAction)
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
            registry.handleKeyboardEvent(event),
            `'${eventString}' should match a keyboard action`
        ).toBe(true)
        v.expect(
            matchedAction,
            `'${eventString}' should match the keyboard action '${action}'`
        ).toBeCalledWith(action)
    } else {
        v.expect(
            registry.handleKeyboardEvent(event),
            `'${eventString}' should not match any keyboard action`
        ).toBe(false)
    }
})

v.test.each([
    // === Should match ===
    {
        event: new MouseEvent('mousedown', { detail: 2 }),
        action: shortcuts.MouseAction.open,
    },
    {
        event: new MouseEvent('mousedown', { ctrlKey: true, detail: 1 }),
        action: shortcuts.MouseAction.selectAdditional,
    },
    {
        event: new MouseEvent('mousedown', { ctrlKey: true, shiftKey: true, detail: 1 }),
        action: shortcuts.MouseAction.selectAdditionalRange,
    },
    {
        event: new MouseEvent('mousedown', { shiftKey: true, detail: 1 }),
        action: shortcuts.MouseAction.selectRange,
    },
    {
        event: new MouseEvent('mousedown', { ctrlKey: true, detail: 1 }),
        action: shortcuts.MouseAction.editName,
    },
    // Triple click or double click instead of single click SHOULD match
    {
        event: new MouseEvent('mousedown', { ctrlKey: true, detail: 3 }),
        action: shortcuts.MouseAction.selectAdditional,
    },
    {
        event: new MouseEvent('mousedown', { ctrlKey: true, detail: 2 }),
        action: shortcuts.MouseAction.selectAdditional,
    },
    // Triple click instead of double click SHOULD match
    {
        event: new MouseEvent('mousedown', { detail: 3 }),
        action: shortcuts.MouseAction.open,
    },
    // === Should not match ===
    // Single click instad of double click
    {
        event: new MouseEvent('mousedown', { detail: 1 }),
        action: shortcuts.MouseAction.open,
        match: false,
    },
    // 0 clicks instad of double click
    {
        event: new MouseEvent('mousedown', { detail: 0 }),
        action: shortcuts.MouseAction.open,
        match: false,
    },
    {
        event: new MouseEvent('mousedown'),
        action: shortcuts.MouseAction.open,
        match: false,
    },
    // Missing modifier keys
    {
        event: new MouseEvent('mousedown', { ctrlKey: true, detail: 2 }),
        action: shortcuts.MouseAction.selectAdditionalRange,
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
        action: shortcuts.MouseAction.selectAdditional,
        match: false,
    },
    {
        event: new MouseEvent('mousedown', { ctrlKey: true }),
        action: shortcuts.MouseAction.open,
        match: false,
    },
])('Mouse shortcut handling', ({ event, action, match = true }) => {
    const registry = shortcuts.ShortcutRegistry.createWithDefaults()
    v.expect(
        registry.matchesMouseAction(action, event),
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
