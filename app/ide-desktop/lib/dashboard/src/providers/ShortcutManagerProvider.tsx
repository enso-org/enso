/** @file The React provider for keyboard and mouse shortcutManager, along with hooks to use the provider
 * via the shared React context. */
import * as React from 'react'

import ShortcutManager from '#/utilities/ShortcutManager'

// ==============================
// === ShortcutManagerContext ===
// ==============================

/** State contained in a `ShortcutsContext`. */
export interface ShortcutManagerContextType {
  shortcutManager: ShortcutManager
}

const ShortcutManagerContext = React.createContext<ShortcutManagerContextType>({
  shortcutManager: ShortcutManager.createWithDefaults(),
})

/** Props for a {@link ShortcutsProvider}. */
export interface ShortcutsProviderProps extends React.PropsWithChildren<object> {
  shortcutManager?: ShortcutManager
}

// ===============================
// === ShortcutManagerProvider ===
// ===============================

/** A React Provider that lets components get the shortcut registry. */
export default function ShortcutsProvider(props: ShortcutsProviderProps) {
  const { shortcutManager: rawShortcutManager, children } = props
  const [shortcutManager, setShortcutManager] = React.useState(
    () => rawShortcutManager ?? ShortcutManager.createWithDefaults()
  )

  React.useEffect(() => {
    if (rawShortcutManager != null) {
      setShortcutManager(rawShortcutManager)
    } else {
      setShortcutManager(ShortcutManager.createWithDefaults())
    }
  }, [rawShortcutManager])

  return (
    <ShortcutManagerContext.Provider value={{ shortcutManager }}>
      {children}
    </ShortcutManagerContext.Provider>
  )
}

/** Exposes a property to get the shortcut registry. */
export function useShortcutManager() {
  return React.useContext(ShortcutManagerContext)
}
