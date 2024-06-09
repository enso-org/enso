import { useKeyboard } from '@/composables/keyboard'
import { createContextStore } from '@/providers'

export { injectFn as injectKeyboard, provideFn as provideKeyboard }

const { provideFn, injectFn } = createContextStore('Keyboard watcher', () => useKeyboard())
