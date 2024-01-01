import { createContextStore } from '@/providers'
import type { ApplicationConfigValue } from '@/util/config'
import { identity } from '@vueuse/core'
import { type Ref } from 'vue'

export type GuiConfig = ApplicationConfigValue

export { injectFn as injectGuiConfig, provideFn as provideGuiConfig }
const { provideFn, injectFn } = createContextStore(
  'GUI config',
  identity<Ref<ApplicationConfigValue>>,
)
