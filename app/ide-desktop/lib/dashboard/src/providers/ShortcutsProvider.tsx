/** @file The React provider for keyboard and mouse shortcuts, along with hooks to use the provider
 * via the shared React context. */
import * as React from 'react'

import * as shortcutsModule from '#/utilities/shortcuts'

// ========================
// === ShortcutsContext ===
// ========================

/** State contained in a `ShortcutsContext`. */
export interface ShortcutsContextType {
    shortcuts: shortcutsModule.ShortcutRegistry
}

// @ts-expect-error The default value will never be exposed as using this without a `Provider`
// is a mistake.
const ShortcutsContext = React.createContext<ShortcutsContextType>(null)

/** Props for a {@link ShortcutsProvider}. */
export interface ShortcutsProviderProps extends React.PropsWithChildren<object> {
    shortcuts?: shortcutsModule.ShortcutRegistry
}

// =========================
// === ShortcutsProvider ===
// =========================

/** A React Provider that lets components get the shortcut registry. */
export default function ShortcutsProvider(props: ShortcutsProviderProps) {
    const { shortcuts: rawShortcuts, children } = props
    const [shortcuts] = React.useState(
        () => rawShortcuts ?? shortcutsModule.ShortcutRegistry.createWithDefaults()
    )

    return <ShortcutsContext.Provider value={{ shortcuts }}>{children}</ShortcutsContext.Provider>
}

/** Exposes a property to get the shortcut registry. */
export function useShortcuts() {
    return React.useContext(ShortcutsContext)
}
