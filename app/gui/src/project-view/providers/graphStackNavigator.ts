import { useStackNavigator } from '@/composables/stackNavigator'
import { createContextStore } from '@/providers'

export const [provideStackNavigator, injectStackNavigator] = createContextStore(
  'graph stack navigator',
  useStackNavigator,
)
