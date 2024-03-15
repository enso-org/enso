import type { KeyboardComposable } from '@/composables/keyboard'
import { useNavigator } from '@/composables/navigator'
import { createContextStore } from '@/providers'
import type { Ref } from 'vue'

export type GraphNavigator = ReturnType<typeof injectFn>
export { injectFn as injectGraphNavigator, provideFn as provideGraphNavigator }
const { provideFn, injectFn } = createContextStore('graph navigator', useNavigator)
