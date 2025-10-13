import { useGlobalKeyboard, useLocalKeyboard } from '@/composables/keyboard'
import { createContextStore } from '@/providers'
import { type GlobalEventRegistry } from './globalEventRegistry'

export const [provideKeyboard, injectKeyboard] = createContextStore(
  'Global keyboard modifier state',
  (globalEventRegistry: GlobalEventRegistry) => useGlobalKeyboard(globalEventRegistry),
)

export const [provideBubblingKeyboard, injectBubblingKeyboard] = createContextStore(
  'Bubbling keyboard modifier state',
  (globalEventRegistry: GlobalEventRegistry) =>
    useLocalKeyboard(globalEventRegistry, document.body),
)
