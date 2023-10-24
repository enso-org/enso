import { useNavigator } from '@/util/navigator'
import { createContextStore } from '.'

export type GraphNavigator = ReturnType<typeof injectFn>
export { injectFn as injectGraphNavigator, provideFn as provideGraphNavigator }
const { provideFn, injectFn } = createContextStore('graph navigator', useNavigator)
