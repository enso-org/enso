import { useStackNavigator } from '@/composables/stackNavigator'
import { createContextStore } from '@/providers'

export { injectFn as injectStackNavigator, provideFn as provideStackNavigator }
const { provideFn, injectFn } = createContextStore('graph stack navigator', useStackNavigator)
