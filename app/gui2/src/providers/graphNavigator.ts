import { createContextStore } from '@/providers'
import { useNavigator } from '@/util/vue/navigator'

export type GraphNavigator = ReturnType<typeof injectFn>
export { injectFn as injectGraphNavigator, provideFn as provideGraphNavigator }
const { provideFn, injectFn } = createContextStore('graph navigator', useNavigator)
