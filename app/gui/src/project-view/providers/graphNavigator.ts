import { useNavigator } from '@/composables/navigator'
import { createContextStore } from '@/providers'

export type GraphNavigator = ReturnType<typeof injectGraphNavigator>
export const [provideGraphNavigator, injectGraphNavigator] = createContextStore(
  'graph navigator',
  useNavigator,
)
