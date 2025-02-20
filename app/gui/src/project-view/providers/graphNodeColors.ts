import { useNodeColors } from '@/composables/nodeColors'
import { createContextStore } from '@/providers'

export const [provideNodeColors, injectNodeColors] = createContextStore(
  'node colors',
  useNodeColors,
)
