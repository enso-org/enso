import { useNodeCreation } from '@/composables/nodeCreation'
import { createContextStore } from '@/providers'

export const [provideNodeCreation, injectNodeCreation] = createContextStore(
  'node creation',
  useNodeCreation,
)
