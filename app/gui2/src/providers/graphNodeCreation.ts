import { useNodeCreation } from '@/composables/nodeCreation'
import { createContextStore } from '@/providers'

export { injectFn as injectNodeCreation, provideFn as provideNodeCreation }
const { provideFn, injectFn } = createContextStore('node creation', useNodeCreation)
