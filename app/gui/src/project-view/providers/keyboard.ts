import { useGlobalKeyboard, useLocalKeyboard } from '@/composables/keyboard'
import { createContextStore } from '@/providers'

export const [provideKeyboard, injectKeyboard] = createContextStore(
  'Global keyboard modifier state',
  () => useGlobalKeyboard(),
)

export const [provideBubblingKeyboard, injectBubblingKeyboard] = createContextStore(
  'Bubbling keyboard modifier state',
  () => useLocalKeyboard(document.body),
)
