import { useNavigator } from '@/composables/navigator'
import { createContextStore } from '@/providers'

export type GraphNavigator = ReturnType<typeof injectFn>
export { injectFn as injectGraphNavigator, provideFn as provideGraphNavigator }
const { provideFn, injectFn } = createContextStore('graph navigator', useNavigator)
