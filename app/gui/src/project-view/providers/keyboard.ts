import { useKeyboard } from '@/composables/keyboard'
import { createContextStore } from '@/providers'

export const [provideKeyboard, injectKeyboard] = createContextStore('Keyboard watcher', () =>
  useKeyboard(),
)
