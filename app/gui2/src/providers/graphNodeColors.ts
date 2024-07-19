import { useNodeColors } from '@/composables/nodeColors'
import { createContextStore } from '@/providers'

export { injectFn as injectNodeColors, provideFn as provideNodeColors }
const { provideFn, injectFn } = createContextStore('node colors', useNodeColors)
